.assembly extern mscorlib
{
  .ver 2:0:0:0
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 ) // .z\V.4..
}
.assembly 'Equals'
{
  .custom instance void class [mscorlib]System.Runtime.CompilerServices.RuntimeCompatibilityAttribute::'.ctor'() =  (
		01 00 01 00 54 02 16 57 72 61 70 4E 6F 6E 45 78   // ....T..WrapNonEx
		63 65 70 74 69 6F 6E 54 68 72 6F 77 73 01       ) // ceptionThrows.

  .hash algorithm 0x00008004
  .ver  0:0:0:0
}
.module Equals.exe // GUID = {9CD6377A-594E-4655-9B1F-565FC3BF5A84}


  .class private auto ansi beforefieldinit Equals
  	extends [mscorlib]System.Object
  {

    // method line 1
    .method public hidebysig specialname rtspecialname 
           instance default void '.ctor' ()  cil managed 
    {
        // Method begins at RVA 0x2050
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call instance void object::'.ctor'()
	IL_0006:  ret 
    } // end of method Equals::.ctor

    // method line 2
    .method private static hidebysig 
           default void println (object msg)  cil managed 
    {
        // Method begins at RVA 0x2058
	// Code size 7 (0x7)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  call void class [mscorlib]System.Console::WriteLine(object)
	IL_0006:  ret 
    } // end of method Equals::println

    // method line 3
    .method public hidebysig 
           instance default bool eqls (object that)  cil managed 
    {
        // Method begins at RVA 0x2060
	// Code size 56 (0x38)
	.maxstack 8
	IL_0000:  ldarg.0 
	IL_0001:  brtrue IL_0015

	IL_0006:  ldstr "this = null"
	IL_000b:  call void class Equals::println(object)
	IL_0010:  br IL_0025

	IL_0015:  ldstr "this = "
	IL_001a:  ldarg.0 
	IL_001b:  call string string::Concat(object, object)
	IL_0020:  call void class Equals::println(object)
	IL_0025:  ldarg.0 
	IL_0026:  brtrue IL_0030

	IL_002b:  ldarg.1 
	IL_002c:  ldnull 
	IL_002d:  ceq 
	IL_002f:  ret 
	IL_0030:  ldarg.0 
	IL_0031:  ldarg.1 
	IL_0032:  callvirt instance bool object::Equals(object)
	IL_0037:  ret 
    } // end of method Equals::eqls

    // method line 4
    .method public static hidebysig 
           default void Main (string[] args)  cil managed 
    {
        // Method begins at RVA 0x209c
	.entrypoint
	// Code size 75 (0x4b)
	.maxstack 2
	.locals init (
		class Equals	V_0,
		class Equals	V_1)
	IL_0000:  ldnull 
	IL_0001:  stloc.0 
	IL_0002:  newobj instance void class Equals::'.ctor'()
	IL_0007:  stloc.1 
	IL_0008:  ldloc.0 
	IL_0009:  call void class Equals::println(object)
	IL_000e:  ldloc.1 
	IL_000f:  call void class Equals::println(object)
	IL_0014:  ldstr "e1 eqls eN"
	IL_0019:  call void class Equals::println(object)
	IL_001e:  ldloc.1 
	IL_001f:  ldloc.0 
	IL_0020:  call instance bool class Equals::eqls(object)
	IL_0025:  box [mscorlib]System.Boolean
	IL_002a:  call void class Equals::println(object)
	IL_002f:  ldstr "eN eqls e1"
	IL_0034:  call void class Equals::println(object)
	IL_0039:  ldloc.0 
	IL_003a:  ldloc.1 
	IL_003b:  call instance bool class Equals::eqls(object)
	IL_0040:  box [mscorlib]System.Boolean
	IL_0045:  call void class Equals::println(object)
	IL_004a:  ret 
    } // end of method Equals::Main

  } // end of class Equals

