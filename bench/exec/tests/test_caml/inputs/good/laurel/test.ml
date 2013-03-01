
let () =
  World.load_export "/usr/local/jc21/api21/java/lang/javacard/lang.exp"

module JavaLang =
  World.JavaLang (struct end)

let () =
  World.load_export "/usr/local/jc21/api21/javacard/framework/javacard/framework.exp";
  World.load_export "/usr/local/jc21/api21/javacard/security/javacard/security.exp";
  World.load_export "/usr/local/jc21/api21/javacardx/crypto/javacard/crypto.exp";
  World.load_export "/usr/local/jc21/samples/com/sun/javacard/samples/SampleLibrary/javacard/SampleLibrary.exp";
  World.load_cap    "/usr/local/jc21/samples/com/sun/javacard/samples/JavaPurse/javacard/JavaPurse.cap"
                    "com/sun/javacard/samples/JavaPurse"

let () =
  World.test()

