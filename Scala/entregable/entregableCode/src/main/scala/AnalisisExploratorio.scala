
object AnalisisExploratorio extends Analizador {
  // val -> inmutables // var -> mutables (no cabia el tipo)

  val dataset = Utilidades.readFile(fichero = "src/adult.data.clean.csv")
  // Implementa la función
  // ejercicio-1:
  // Número total de registros en el dataset.
  def totalDeRegistros(c: Seq[Contribuyente]): Int = c.length

  // ejercicio-2:
  // Calcular la media de edad de todos los contribuyentes
  //OVERFLOW (Stack Overflow - demasiados registros)
  def calculaEdadMediaRec(c: Seq[Contribuyente]): Double = {
   val  mean = if(c.length==1) c(0).age else (calculaEdadMediaRec(c.tail) * (c.length-1) + c(0).age)/(c.length)
    mean
  }
  // debido a que el programa recursivo presenta overflow lo he hecho de forma tradicional con bucles
  def calculaEdadMedia(c: Seq[Contribuyente]): Double = {
    // primer valor
    var mean = c(0).age.toDouble
    // resto de valores
    for (i <- 1 to c.length - 1) mean = (mean * i + c(i).age) / (i+1)
    mean
  }

  // ejercicio-3:
  // Calcular la media de edad de todos los contribuyentes sin contar aquellos cuya edad sea 0
  def calculaEdadMediaNoZeros(c: Seq[Contribuyente]): Double = {
    // filter array
    val cFiltered = c.filter(x =>  x.age > 0)
    // calcular edad media con array filtrado
    calculaEdadMedia(c = cFiltered)
  }

  // Implementa la función
  // ejercicio-4:
  // Descubrir de cuántos países distintos provienen los contribuyentes
  def paisesOrigenUnicos(c: Seq[Contribuyente]): Seq[String] = {
    // seq of nativeCountry
    var countries = c.map(x => x.nativeCountry)
    // toSet para eliminar rep
    countries = countries.toSet.toSeq
    countries
  }

  // Implementa la función
  // ejercicio-5:
  // De todos los contribuyentes, ¿cómo se distribuye por género?. Devuelve el porcentaje de hombres
  // y el de mujeres en ese orde, (porcentajeDeHombres, porcentajeDeMujeres)
  def distribucionPorGeneros(c: Seq[Contribuyente]): (Double, Double) = {
    // num men
    val numMen = c.filter(x =>  x.sex == "Male").length.toDouble
    // paro obtener 2 decimales multipicamos por 100, redondeamos y dividimos entre 100
    (
      (math rint (
        (numMen/c.length)*100
        ) *100)/100,
      (math rint (
        ((c.length-numMen)/c.length)*100
        ) *100)/100
    )
  }

  // Implementa la función
  // ejercicio-6:
  // Cuál es el tipo de trabajo (workclass) cuyos ingresos son mayoritariamente superiores a ">50K
  def trabajoMejorRemunerado(c: Seq[Contribuyente]): String = {
    // filter
    val cFiltered = c.filter(x =>  x.income == "<=50K")
    // seq of workclass
    val workclasses = c.map(x => x.workclass)
    // map of ('workclass' -> numRep)
    val workclassesMap = workclasses.groupBy(identity).view.mapValues(_.size)
    // get max value (numRep)
    val maxPair = workclassesMap.maxBy(_._2) //{ case (key, value) => value }
    maxPair._1 // key
    //maxPair.toString
  }

  // Implementa la función
  // ejercicio-7:
  // Cuál es la media de años de educación (education-num) de aquellos contribuyentes cuyo país de origen no es
  // United-States
  def aniosEstudiosMedio(c: Seq[Contribuyente]): Double = {
    // recursiva para años medio
    // en ests funcion no se da OVERFLOW ta que disminuye le numero de registros
    def getMeanYears(c: Seq[Contribuyente]): Double = {
      if(c.length==1) c(0).educationNum
      else (getMeanYears(c.tail) * (c.length-1) + c(0).educationNum)/(c.length)
    }
    // filter
    val cFiltered = c.filter(x =>  x.nativeCountry != "United-States")
    // llamada a la funcion recursiva
    val mean = getMeanYears(c = cFiltered)
    mean
  }

  // CONSOLE OUTPUT
  println(s" -> Dataset tiene un total de registros: ${totalDeRegistros(c = dataset)}")
  println(s" -> En el dataset, los contribuyentes tienen una edad media: ${calculaEdadMedia(c = dataset)}")
  println(s" -> En el dataset, los contribuyentes tienen una edad media (sin contar aquellos con age = 0): ${calculaEdadMediaNoZeros(c = dataset)}")
  println(s" -> Los contribuyentes proviende de distintos países como: ${paisesOrigenUnicos(c = dataset).foreach(println)}")
  println(s" -> Los contribuyentes se distribuyen en (hombres - mujeres): ${distribucionPorGeneros(c = dataset)}")
  println(s" -> El tipo de trabajo mejor remunerado en el dataset es: ${trabajoMejorRemunerado(c = dataset)}")
  println(s" -> La media de años de estudio de los contribuyenes de origen distinto a United States es: ${aniosEstudiosMedio(c = dataset)}")

  // ejercicio-11
  println(" -> Todos los contribullentes del dataset:")
  //imprimeContribuyentes(dataset.take(100))
  imprimeContribuyentes(dataset)

}
