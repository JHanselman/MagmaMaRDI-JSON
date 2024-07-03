//Define JSON constants

JSON_COMMA := ",";
JSON_COLON := ":";
JSON_LEFTBRACKET := "[";
JSON_RIGHTBRACKET := "]";
JSON_LEFTBRACE := "{";
JSON_RIGHTBRACE := "}";
JSON_QUOTE := "\"";

JSON_WHITESPACE := [" ", "\t", "\b", "\n", "\r"];
JSON_SYNTAX := [JSON_COMMA, JSON_COLON, JSON_LEFTBRACKET, JSON_RIGHTBRACKET,
               JSON_LEFTBRACE, JSON_RIGHTBRACE];

FALSE_LEN := #"false";
TRUE_LEN := #"true";
NULL_LEN := #"null";

//Set up key names

type_key := "_type";
refs_key := "_refs";

//Define global variables

get, set := NewEnv(["encode_type", "reverse_type_map", "serialize_params", "serialize_id", "id_to_obj", "is_singleton", "save_object", "save_type_params", "load_object", "load_type_params"]);

set("encode_type", AssociativeArray());
set("reverse_type_map", AssociativeArray());
set("serialize_params", AssociativeArray());
set("serialize_id", AssociativeArray());
set("id_to_obj", AssociativeArray());
set("is_singleton", AssociativeArray());
set("save_object", AssociativeArray());
set("save_type_params", AssociativeArray());
set("load_object", AssociativeArray());
set("load_type_params", AssociativeArray());


// Setup all types supported by the serializer/deserializer

//Caution: With double assignment only the last one will be used in the reverse_type_map
L:=[];

//Rings:
L cat:= [[* "ZZRing", RngInt, false, false, true*], [* "QQField", FldRat, false, false, true*], [*"PolyRing", RngUPol, false, true, false*], [*"MPolyRing", RngMPol, false, true, false *]];

//Fields
L cat:= [[* "FqField", FldFin, false, true, false*]];

//Matrix spaces
//TODO: This is going to cause problems
L cat:= [[* "MatSpace", ModMatFld, false, true, false*], [* "MatSpace", AlgMat, false, true, false*], [* "MatSpace", ModMatRng, false, true, false*]];

//Basic elements
L cat:= [[* "String", MonStgElt, false, false, false*], [* "ZZRingElem", RngIntElt, false, false, false*], [* "Bool", BoolElt, false, false, false*], [* "QQFieldElem", FldRatElt, false, false, false*]];

//Arrays and dictionaries
L cat:= [[* "Vector", SeqEnum, true, false, false *], [* "Dict", Assoc, true, false, false *]];

//Polynomial elements
L cat:= [[* "PolyRingElem", RngUPolElt, true, false, false *], [* "MPolyRingElem", RngMPolElt, true, false, false *]];

//Field elements
L cat:= [[* "FqFieldElem", FldFinElt, true, false, false*]];

//Matrices
//TODO: This is going to cause problems. Currently putting ModMatRngElt last because it seems the most general. Maybe split reverse_type_map.
L cat:= [[* "MatElem", ModMatFldElt, true, false, false*], [* "MatElem", Mtrx, true, false, false*], [* "MatElem", ModMatRngElt, true, false, false*]];

encode_type := get("encode_type");
reverse_type_map := get("reverse_type_map");
serialize_params := get("serialize_params");
serialize_id := get("serialize_id");
is_singleton := get("is_singleton");

for tup in L do
  str := tup[1];
  T := tup[2];
  encode_type[T] := str;
  reverse_type_map[str] := T;
  serialize_params[T] := tup[3];
  serialize_id[T] := tup[4];
  is_singleton[T] := tup[5];
  if tup[4] then
    AddAttribute(T, "uuid");
  end if;
end for;


set("encode_type", encode_type);
set("reverse_type_map", reverse_type_map);
set("serialize_params", serialize_params);
set("serialize_id", serialize_id);
set("is_singleton", is_singleton);
