let norm a = Int64.logand a 0xffffffffL
let norm_shift_left a b = norm (Int64.shift_left a b)
let cn c = Int64.of_int (Char.code c)

(*** sha256 code ported from Brad Conte's C version: https://github.com/B-Con/crypto-algorithms (brad@bradconte.com) http://bradconte.com/sha256_c ***)
let dbl_int_add(bl,c) =
  if Int64.compare bl.(0) (Int64.sub 0xffffffffL c) > 0 then
    bl.(1) <- Int64.add 1L bl.(1)
  else
    bl.(0) <- Int64.add bl.(0) c

let rotleft(a,b) =
  Int64.logor (norm_shift_left a b) (Int64.shift_right_logical a (32-b))

let rotright(a,b) =
  Int64.logor (norm_shift_left a (32-b)) (Int64.shift_right_logical a b)

let ch(x,y,z) = Int64.logxor (Int64.logand x y) (Int64.logand (Int64.logxor x 0xffffffffL) z)

let maj(x,y,z) = Int64.logxor (Int64.logand x y) (Int64.logxor (Int64.logand x z) (Int64.logand y z))

let ep0(x) = Int64.logxor (rotright(x,2)) (Int64.logxor (rotright(x,13)) (rotright(x,22)))
let ep1(x) = Int64.logxor (rotright(x,6)) (Int64.logxor (rotright(x,11)) (rotright(x,25)))
let sig0(x) = Int64.logxor (rotright(x,7)) (Int64.logxor (rotright(x,18)) (Int64.shift_right_logical x 3))
let sig1(x) = Int64.logxor (rotright(x,17)) (Int64.logxor (rotright(x,19)) (Int64.shift_right_logical x 10))

let data = Array.make 64 'a'
let datalen = ref 0
let bitlen = Array.make 2 0L
let state = Array.make 8 0L

let k =
  [|0x428a2f98L;0x71374491L;0xb5c0fbcfL;0xe9b5dba5L;0x3956c25bL;0x59f111f1L;0x923f82a4L;0xab1c5ed5L;
    0xd807aa98L;0x12835b01L;0x243185beL;0x550c7dc3L;0x72be5d74L;0x80deb1feL;0x9bdc06a7L;0xc19bf174L;
    0xe49b69c1L;0xefbe4786L;0x0fc19dc6L;0x240ca1ccL;0x2de92c6fL;0x4a7484aaL;0x5cb0a9dcL;0x76f988daL;
    0x983e5152L;0xa831c66dL;0xb00327c8L;0xbf597fc7L;0xc6e00bf3L;0xd5a79147L;0x06ca6351L;0x14292967L;
    0x27b70a85L;0x2e1b2138L;0x4d2c6dfcL;0x53380d13L;0x650a7354L;0x766a0abbL;0x81c2c92eL;0x92722c85L;
    0xa2bfe8a1L;0xa81a664bL;0xc24b8b70L;0xc76c51a3L;0xd192e819L;0xd6990624L;0xf40e3585L;0x106aa070L;
    0x19a4c116L;0x1e376c08L;0x2748774cL;0x34b0bcb5L;0x391c0cb3L;0x4ed8aa4aL;0x5b9cca4fL;0x682e6ff3L;
    0x748f82eeL;0x78a5636fL;0x84c87814L;0x8cc70208L;0x90befffaL;0xa4506cebL;0xbef9a3f7L;0xc67178f2L|]

let sha256_transform() =
  let a = ref state.(0) in
  let b = ref state.(1) in
  let c = ref state.(2) in
  let d = ref state.(3) in
  let e = ref state.(4) in
  let f = ref state.(5) in
  let g = ref state.(6) in
  let h = ref state.(7) in
  let i = ref 0 in
  let j = ref 0 in
  let t1 = ref 0L in
  let t2 = ref 0L in
  let m = Array.make 64 0L in
  while (!i < 16) do
    m.(!i) <- norm (Int64.logor (norm_shift_left (cn data.(!j)) 24) (Int64.logor (norm_shift_left (cn data.(!j + 1)) 16) (Int64.logor (norm_shift_left (cn data.(!j + 2)) 8) (cn data.(!j + 3)))));
    incr i;
    j := !j + 4
  done;
  while (!i < 64) do
    m.(!i) <- norm (Int64.add (sig1 m.(!i-2)) (Int64.add m.(!i-7) (Int64.add (sig0 m.(!i-15)) m.(!i-16))));
    incr i
  done;
  i := 0;
  while (!i < 64) do
    t1 := norm (Int64.add !h (Int64.add (ep1 !e) (Int64.add (ch(!e,!f,!g)) (Int64.add (k.(!i)) (m.(!i))))));
    t2 := norm (Int64.add (ep0 !a) (maj(!a,!b,!c)));
    h := !g;
    g := !f;
    f := !e;
    e := norm (Int64.add !d !t1);
    d := !c;
    c := !b;
    b := !a;
    a := norm (Int64.add !t1 !t2);
    incr i
  done;
  state.(0) <- norm (Int64.add state.(0) !a);
  state.(1) <- norm (Int64.add state.(1) !b);
  state.(2) <- norm (Int64.add state.(2) !c);
  state.(3) <- norm (Int64.add state.(3) !d);
  state.(4) <- norm (Int64.add state.(4) !e);
  state.(5) <- norm (Int64.add state.(5) !f);
  state.(6) <- norm (Int64.add state.(6) !g);
  state.(7) <- norm (Int64.add state.(7) !h)

let sha256_init() =
  datalen := 0;
  bitlen.(0) <- 0L;
  bitlen.(1) <- 0L;
  state.(0) <- 0x6a09e667L;
  state.(1) <- 0xbb67ae85L;
  state.(2) <- 0x3c6ef372L;
  state.(3) <- 0xa54ff53aL;
  state.(4) <- 0x510e527fL;
  state.(5) <- 0x9b05688cL;
  state.(6) <- 0x1f83d9abL;
  state.(7) <- 0x5be0cd19L

let sha256_update(da,len) =
  for i = 0 to len-1 do
    data.(!datalen) <- da.(i);
    incr datalen;
    if (!datalen == 64) then
      begin
	sha256_transform();
	dbl_int_add(bitlen,512L);
	datalen := 0
      end
  done

let sha256_update_str(da,len) =
  for i = 0 to len-1 do
    data.(!datalen) <- da.[i];
    incr datalen;
    if (!datalen == 64) then
      begin
	sha256_transform();
	dbl_int_add(bitlen,512L);
	datalen := 0
      end
  done

let sha256_final() =
  let i = ref !datalen in
  if (!datalen < 56) then
    begin
      data.(!i) <- '\128';
      incr i;
      while (!i < 56) do
	data.(!i) <- '\000';
	incr i;
      done
    end
  else
    begin
      data.(!i) <- '\128';
      incr i;
      while (!i < 64) do
	data.(!i) <- '\000';
	incr i;
      done;
      sha256_transform();
      for j = 0 to 55 do
	data.(j) <- '\000'
      done
    end;
  dbl_int_add(bitlen,Int64.of_int (!datalen * 8));
  data.(63) <- Char.chr (Int64.to_int (Int64.logand 0xffL (bitlen.(0))));
  data.(62) <- Char.chr (Int64.to_int (Int64.logand 0xffL (Int64.shift_right_logical (bitlen.(0)) 8)));
  data.(61) <- Char.chr (Int64.to_int (Int64.logand 0xffL (Int64.shift_right_logical (bitlen.(0)) 16)));
  data.(60) <- Char.chr (Int64.to_int (Int64.logand 0xffL (Int64.shift_right_logical (bitlen.(0)) 24)));
  data.(59) <- Char.chr (Int64.to_int (Int64.logand 0xffL (bitlen.(1))));
  data.(58) <- Char.chr (Int64.to_int (Int64.logand 0xffL (Int64.shift_right_logical (bitlen.(1)) 8)));
  data.(57) <- Char.chr (Int64.to_int (Int64.logand 0xffL (Int64.shift_right_logical (bitlen.(1)) 16)));
  data.(56) <- Char.chr (Int64.to_int (Int64.logand 0xffL (Int64.shift_right_logical (bitlen.(1)) 24)));
  sha256_transform();
  Printf.sprintf "%08Lx%08Lx%08Lx%08Lx%08Lx%08Lx%08Lx%08Lx" state.(0) state.(1) state.(2) state.(3) state.(4) state.(5) state.(6) state.(7)

    (*** sha256 of a string, returning a hex string ***)
let sha256 s =
  sha256_init();
  sha256_update_str(s,String.length s);
  let mh = sha256_final() in
  mh

let sha256revh : (string,string) Hashtbl.t = Hashtbl.create 1000
