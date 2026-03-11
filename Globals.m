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

get, set := NewEnv(["type_data","reverse_type_map", "id_to_obj","save_object", "save_type_params", "load_object", "type_params"]);

set("type_data", AssociativeArray());
set("reverse_type_map", AssociativeArray());
set("id_to_obj", AssociativeArray());
set("save_object", AssociativeArray());
set("save_type_params", AssociativeArray());
set("load_object", AssociativeArray());
set("type_params", AssociativeArray());


// Setup all types supported by the serializer/deserializer

//Caution: With double assignment only the last one will be used in the reverse_type_map
L:=[];


function num_field_ser_type(obj)
  if IsAbsoluteField(obj) then
    return 1;
  else
    return 2;
  end if;
end function;

function num_field_elem_ser_type(obj)
  if IsAbsoluteField(Parent(obj)) then
    return 1;
  else
    return 2;
  end if;
end function;

//Format: Magma Type, Serialization Type(s), Has Parameters, Needs uuid, Is Singleton, Serialization decision function

//Rings:
L cat:= [[* RngInt, "ZZRing", false, false, true*], [* FldRat, "QQField", false, false, true*], [*FldRe, "FldRe", false, true, false*], [*FldCom, "FldCom", false, true, false*], [*RngUPol, "PolyRing", false, true, false*], [*RngMPol, "MPolyRing", false, true, false *]];

//Fields
L cat:= [[* FldFin, "FiniteField", false, true, false*], [* FldNum, ["AbsSimpleNumField", "RelSimpleNumberField"], false, true, false, num_field_ser_type*]];

//Matrix spaces
//TODO: This is going to cause problems
L cat:= [[* ModMatFld, "MatSpace", false, true, false*], [* AlgMat, "MatSpace", false, true, false*], [* ModMatRng, "MatSpace", false, true, false*]];

//Basic elements
L cat:= [[* MonStgElt, "String", false, false, false*], [* RngIntElt, "ZZRingElem", false, false, false*], [* BoolElt, "Bool", false, false, false*], [* FldRatElt, "QQFieldElem", false, false, false*]];

//Arrays and dictionaries
L cat:= [[* SeqEnum, "Vector", true, false, false *], [* Assoc, "Dict", true, false, false *]];

//Polynomial elements
L cat:= [[* RngUPolElt, "PolyRingElem", true, false, false *], [* RngMPolElt, "MPolyRingElem", true, false, false *]];

//Field elements
L cat:= [[* FldFinElt, "FqFieldElem", true, false, false*], [* FldNumElt, ["AbsSimpleNumFieldElem", "RelSimpleNumFieldElem"], true, false, false, num_field_elem_ser_type *], [*FldReElt, "FldReElt", true, false, false*], [*FldComElt, "FldComElt", true, false, false*]];

//Orders
L cat:= [[* RngOrd, "SimpleNumFieldOrder", true, true, false*]];

//Order elements
L cat:= [[* RngOrdElt, "SimpleNumFieldOrderElem", true, false, false*]];

//Field elements
L cat:= [[* CrvEll, "EllipticCurve", false, true, false*]];

//Ideals
L cat:= [[* RngOrdIdl, "SimpleNumFieldOrderIdeal", true, false, false*]];

//Matrices
//TODO: This is going to cause problems. Currently putting ModMatRngElt last because it seems the most general. Maybe split reverse_type_map.
L cat:= [[* ModMatFldElt, "MatElem", true, false, false*], [* Mtrx, "MatElem", true, false, false*], [* AlgMatElt, "MatElem" , true, false, false*], [* ModMatRngElt, "MatElem" , true, false, false*]];

type_data := get("type_data");
reverse_type_map := get("reverse_type_map");


for tup in L do
  T := tup[1];
  types := tup[2];
  type_data[T] := Remove(tup,1);

  if Type(types) eq MonStgElt then
    reverse_type_map[types] := T;
  else
    for type in types do
      reverse_type_map[type] := T;
    end for;
  end if;

  if tup[4] then
    AddAttribute(T, "uuid");
  end if;
end for;

set("type_data", type_data);
set("reverse_type_map", reverse_type_map);
