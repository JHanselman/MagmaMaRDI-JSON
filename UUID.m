intrinsic uuid4() -> MonStgElt
{Generate a random uuid}
  s:= RandomBits(128);
  v := 0xFFFFFFFFFFFF0FFF3FFFFFFFFFFFFFFF;
  v2 := 0x00000000000040008000000000000000;
  s:= BitwiseAnd(s,v);
  s:= BitwiseOr(s,v2);
  hex := Sprintf("%h", s);
  if #hex lt 34 then
    d:= 34 - #hex;
    hex := "0x" cat "0"^d cat hex[3..#hex];
  end if;
  //TODO: Pad with zeroes if needed
  res := hex[3..10] cat "-" cat hex[11..14] cat "-" cat hex[15..18] cat "-" cat hex[19..22] cat "-" cat hex[23..34];
  return res;
end intrinsic;

intrinsic is_uuid(str::MonStgElt) -> BoolElt
{Return whether a String is a UUID or not}
  S := Split(str, "-");
  if #S ne 5 then
    return false;
  end if;
  L := [8, 4, 4, 4, 12];
  for i in [1..5] do
    test, part, _ := Regexp("[A-Fa-f0-9]*", S[i]);
    if #part ne L[i] then
      return false;
    end if;
  end for;
  if S[3][1] ne "4" then
    return false;
  end if;
  if not (S[4][1] in "89abAB") then
    return false;
  end if;
  
  return true;
end intrinsic;
