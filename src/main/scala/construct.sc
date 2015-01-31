object construct {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val t1 = StringConstruction.getCTypeOf("int x = 3;")
                                                  //> t1  : CType = PrimitiveType(UNKNOWN,int)

  val t2 = StringConstruction.getCTypeOf("int x[4];")
                                                  //> t2  : CType = ArrayType(x,x_0,4,PrimitiveType(x[x_0],int))

  // Our visitor can correctly order the rows?
  // x[3][4] should be outputArr(3, outputArr(4))
  val t3 = StringConstruction.getCTypeOf("int x[3][4];")
                                                  //> t3  : CType = ArrayType(x,x_0,3,ArrayType(x[x_0],x_1,4,PrimitiveType(x[x_0][
                                                  //| x_1],int)))

  // Likewise, this should be [3][4][5].
  val t4 = StringConstruction.getCTypeOf("int x[3][4][5];")
                                                  //> t4  : CType = ArrayType(x,x_0,3,ArrayType(x[x_0],x_1,4,ArrayType(x[x_0][x_1]
                                                  //| ,x_2,5,PrimitiveType(x[x_0][x_1][x_2],int))))
                                                  
  val t = "Abc"                                   //> t  : String = Abc
  println(s"${t}_of")                             //> Abc_of
  println(s"$t[$t]")                              //> Abc[Abc]
}