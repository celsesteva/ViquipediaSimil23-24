package mr
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import akka.actor.{ActorSystem, PoisonPill, Props}
import akka.pattern.ask

import scala.language.postfixOps
import scala.concurrent.duration._
import scala.concurrent.duration.DurationInt



object MRWrapper{
  def execute[K1,V1,K2,V2,V3](
                               input:List[(K1,List[V1])], //Todo: TO MAP
                               mapping:(K1,List[V1]) => List[(K2,V2)],
                               reducing:(K2,List[V2])=> (K2,V3),
                               mappers: Int = 1,
                               reducers: Int = 1, nomSystema:String ="sistema", nomMaster: String ="master"): Map[K2, V3] = {
    //TODO: MIRAR SI EL IF HA DE SER AQUI.
    if(input.nonEmpty) {
      val systema: ActorSystem = ActorSystem(nomSystema)
      val actor = systema.actorOf(Props(new MR(input, mapping, reducing, mappers, reducers)), name = nomMaster)
      implicit val timeout = Timeout(10000000 seconds)
      val future = actor ? mr.MapReduceCompute()
      val map: Map[K2, V3] = Await.result(future, Duration.Inf).asInstanceOf[Map[K2, V3]]
      systema.terminate()
      map
    }
    else{
      Map[K2,V3]();
    }
  }
}

case class MapReduceCompute()
case class toMapper[K1,V1](fitxer: K1, text: List[V1])
case class fromMapper[K2,V2](intermig: List[(K2,V2)])
case class toReducer[K2,V2](word:K2, fitxers:List[V2])
case class fromReducer[K2,V3](finals: (K2,V3))

// Els actors mappers són polimòrfics ja que  reben la funció de mapping polimòrfica que han d'aplicar
class Mapper[K1,V1,K2,V2](mapping:(K1,List[V1]) => List[(K2,V2)]) extends Actor {
  def receive: Receive = {
    // cal anotar clau:K1 i valor:List[V1] per tal d'instanciar adequadament el missatge toMapper amb les variables de tipus de Mapper
    // Compte, que no us enganyi que hagi donat els mateixos noms a les variables de tipus al Mapper que a les case class de fora. S'han
    // de vincular d'alguna manera perquè sinó l'inferidor de tipus no sap a quin dels paràmetres de tipus del mapper correspon el tipus de la clau
    // i el tipus del valor al missatge toMapper
    case toMapper(clau:K1,valor:List[V1])=>
        sender ! fromMapper(mapping(clau,valor))
  }
}

// Els actors reducers són polimòrfics ja que reben la funció de reducing polimòrfica que han d'aplicar
class Reducer[K2,V2,V3](reducing:(K2,List[V2])=> (K2,V3)) extends Actor {
  def receive: Receive = {
    // cal anotar també la clau i el valor com hem fet amb els mappers
    case toReducer(clau:K2,valor:List[V2])=>
      sender ! fromReducer(reducing(clau, valor))
  }
}

class MR[K1,V1,K2,V2,V3](
                          input:List[(K1,List[V1])],
                          mapping:(K1,List[V1]) => List[(K2,V2)],
                          reducing:(K2,List[V2])=> (K2,V3), //el index del reducer és el de retorn.
                          mappers: Int = 1,
                          reducers: Int = 1) extends Actor {


  /*
  rep 3 mappers i 2 reducers:
  crea 3 mappers a la llista mappers, entre els mappers reparteix tota la llista. Cada valor de la llista
  s'envia a un mapper fent un toMapper. I per tant el MR rebra tants fromMappers com valors a la llista, però només de 3 mmappers diferents.

  crea 2 reducers, entre els reducers es reparteixen dict. S'envia a cada reducer amb toReducer. Cada reducer retornarà un
  fromReducer. Per tant es rebran dict.length reducers.

  Per tant mapperPendents és input.length. L'he renombrat a fromMappersPendents perquè té més sentit, ja que espera tants fromMappers,
  no pas mappers.

  El mateix amb reducersPendents.

  Crec que es demanava això, no pas crear un actor per a enviar un grup de input. (dividir l'input en grups i enviar-lo) a actors nous creats.
   */

  val inputLength = input.length
  var nmappers = Math.max(Math.min(mappers,inputLength),1);
  var maxReducers = reducers;
  var fromMappersPendents = 0
  var nreducers = 0 // adaptar per poder tenir menys reducers
  var fromReducersPendents = 0

  // dict serà el diccionari amb el resultat intermedi
  var dict: Map[K2, List[V2]] = Map[K2, List[V2]]() withDefaultValue List()
  // resultatFinal recollirà les respostes finals dels reducers
  var resultatFinal: Map[K2, V3] = Map()

  // Ens apuntem qui ens ha demanat la feina
  var client:ActorRef = null



  def receive: Receive = {

    // En rebre el missatge MapReduceCompute engeguem el procés.
    case MapReduceCompute() =>
      client = sender() // Ens apuntem qui ens ha fet l'encàrrec per enviar-li el missatge més tard.

      // farem un mapper per parella (K1,List[V1]) de l'input
      // nmpapper = input.length


      // Al crear actors a dins d'altres actors, enlloc de crear el ActorSystem utilitzarem context i així es va
      // organitzant la jerarquia d'actors.
      // D'altra banda, quan els actors que creem tenen un constructor amb paràmetre, no passem el "tipus" de l'actor i prou
      // a Props sino que creem l'actor amb els paràmetres que necessita. En aquest cas, l'Actor mapping és paramètric en tipus
      // i necessita com a paràmetre una funció de mapping.

      val mappers: Seq[ActorRef] =
        for (i <- 0 until nmappers) yield
        {
          context.actorOf(Props(new Mapper(mapping)), "mapper" + i)
        }
      // No és necessari passar els tipus K1,V1, ... ja que els infereix SCALA pel paràmetre mapping
      // D'altra banda compte pq  "0 until n" és el mateix que "0 to n-1".
      // val mappers = for (i <- 0 to nmappers-1) yield {
      //      context.actorOf(Props(new Mapper[K1,V1,K2,V2](mapping)), "mapper"+i)
      // }

      // Posar les anotacions de tipus aquí no és necessari però ajuda a llegir que
      // a cada mapper li enviem una clau de tipus K1 i una llista de valors de tipus V1

      // Aquesta versi és quadràtica perquè l'accés a input(i) te cost i mentre
      // que la versió de sota amb el zipWithIndex senzillament recorre l'input un cop
      // linealment. AQUEST CANVI ÉS CLAU EN EL RENDIMENT
      //for(i<- 0 until nmappers) mappers(i) ! toMapper(input(i)._1:K1, input(i)._2: List[V1])

      for(((p1,p2),i)<- input.zipWithIndex) {
          mappers(i % nmappers) ! toMapper(p1: K1, p2 :List[V1]) //mappers.foreach(_ ! toMapper(p1: K1, p2 :List[V1]))
      }

      // Per tant, alternativament...
      // for(i<- 0 until nmappers) mappers(i) ! toMapper(input(i)._1, input(i)._2)

      // Necessitem controlar quant s'han acabat tots els mappers per poder llençar els reducers després...
      fromMappersPendents = inputLength



    // Anem rebent les respostes dels mappers i les agrupem al diccionari per clau.
    // Tornem a necessitar anotar els tipus del paràmetre que reb fromMapper tal com ho hem fet
    // o be en el codi comentat al generar les tuples.

    case fromMapper(list_clau_valor:List[(K2,V2)]) =>
      for ((clau, valor) <- list_clau_valor)
        dict += (clau -> (valor :: dict(clau)))

      fromMappersPendents -= 1

      //TODO també peta quan selecciones 2 1 a 1
      // Quan ja hem rebut tots els missatges dels mappers:
      if (fromMappersPendents==0)
      {
        // creem els reducers, tants com entrades al diccionari; fixeu-vos de nou que fem servir context i fem el new
        // pel constructor del Reducer amb paràmetres
        nreducers = Math.max(Math.min(dict.size,maxReducers),1);
        fromReducersPendents = dict.size // actualitzem els reducers pendents
        if(dict.nonEmpty) {
        //TODO: quan hi ha 0 (dict.size == 0) es queda penjat. Això quan (A,List()), es penja amb test_viqui
          val reducers = for (i <- 0 until nreducers) yield
            context.actorOf(Props(new Reducer(reducing)), "reducer" + i)

        // No cal anotar els tipus ja que els infereix de la funció reducing
        //context.actorOf(Props(new Reducer[K2,V2,V3](reducing)), "reducer"+i)

        // Ara enviem a cada reducer una clau de tipus V2 i una llista de valors de tipus K2. Les anotacions de tipus
        // no caldrien perquè ja sabem de quin tipus és dict, però ens ajuden a documentar.

          for (((key: K2, lvalue: List[V2]), i) <- dict.zipWithIndex) { //TODO: es penja pq dict és buit.
            reducers(i % nreducers) ! toReducer(key, lvalue)
          }
        }
        else{ //dict.size == 0, fromRreducersPenders = 0,
          client ! Map.empty[K2, V3]
          context.stop(self)
        }
      }

    // A mesura que anem rebent respostes del reducer, tuples (K2, V3), les anem afegint al Map del resultatfinal i
    // descomptem reducers pendents. Tornem a necessitar anotar el tipus.
    case fromReducer(entradaDictionari:(K2,V3)) =>
      resultatFinal += entradaDictionari
      fromReducersPendents -= 1


      // En arribar a 0 enviem a qui ens ha encarregat el MapReduce el resultat. De fet l'està esperant pq haurà fet un ask.
      if (fromReducersPendents == 0) {
        client ! resultatFinal
        // Ara podem alliberar els recursos dels actors, el propi MapReduce, tots els mappers i els reducers.
        // Fixem-nos que no te sentit que tornem a engegar "aquest mateix" MapReduce.
        context.stop(self)
      }
  }

}