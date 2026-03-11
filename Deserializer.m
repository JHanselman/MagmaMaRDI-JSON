import "Globals.m":type_key, refs_key, get, set; 
import "Serializer.m":set_key, set_params, is_singleton, serialize_id, serialize_params; 

//Define DeserializerState object

declare type DeserState;
declare attributes DeserState: obj, key, refs;

intrinsic Print(X::DeserState)
{Print X}
  printf("DeserializerState for JSON object");
end intrinsic;

intrinsic DeserializerState(obj::Assoc, key::MonStgElt, refs::Assoc) -> DeserState
{Initialize serializer state}
  S := New(DeserState);
  S`obj := obj;
  S`key := key;
  S`refs := refs;
  return S;
end intrinsic;

//Constructor 

function deserializer_open(io)
  obj := json_parse(Read(io));
  refs := AssociativeArray();
  if refs_key in Keys(obj) then
    refs := obj[refs_key];
  end if;

  return DeserializerState(obj, "", refs);
end function;

//Key deserialization functions

forward load_ref;

function load_node(f, s: key:= "")
  obj_uuid := s`obj;
  if Type(obj_uuid) eq MonStgElt and is_uuid(obj_uuid) then
    return load_ref(s);
  end if;
  
  //TODO: Hacked together? Recognize whether the key is an integer -> array entry or a String -> find value belonging to key.
  if Type(key) eq RngIntElt then
     obj := s`obj;
     s`obj := s`obj[key];
     s`key := "";
     result := f(s`obj);
     s`obj := obj;
     return result;
  end if;
  
  if not (key eq "") then
    set_key(s, key);
  end if;
  obj := s`obj;

  if not (s`key eq "") then
    s`obj := s`obj[s`key];
  end if;
  s`key := "";
  result := f(s`obj);
  s`obj := obj;
  return result;
end function;

function decode_type(s)
  if Type(s`obj) eq MonStgElt then 
    if is_uuid(s`obj) then
      obj_id := s`obj;
      obj := s`obj;
      if #s`refs eq 0 then
        id_to_obj := get("id_to_obj");
        return Type(id_to_obj[obj_id]);
      end if;
      s`obj := s`refs[obj_id];
      T := decode_type(s);
      s`obj := obj;
      return T;
    end if;
    reverse_type_map := get("reverse_type_map");
    if s`obj in Keys(reverse_type_map) then
      return reverse_type_map[s`obj];
    else
      Error("unsupported type " cat Sprint(s`obj) cat  " for decoding");
    end if;
  end if;

  if type_key in Keys(s`obj) then
     g := function(x);
       return decode_type(s);
    end function;
    return load_node(g, s: key := type_key);
  end if;

  if "name" in Keys(s`obj) then
    g := function(x);
      return decode_type(s);
    end function;
    return load_node(g, s: key :="name");
  end if;
  return decode_type(s`obj);
end function;


//Generic load functions

function load_object(s, tp)
  T:= tp`type;
  f:= get("load_object")[T];
  //TODO: Hack for finite field case. Redesign this later.
  if T eq FldFin then
    return f(s, tp);
  elif assigned tp`params then
    return f(s, tp`params);
  else
    return f(s);
  end if;
end function;

function load_object_with_key(s, tp, key)
  g := function(x)
    return load_object(s, tp);
  end function;
  return load_node(g, s : key := key);
end function;


function load_array_node(f, s: key:="")
   g := function(s, array)
     L := [];
     for i in [1..#array] do
       function helpfunction(x)
         return f((i,x));
       end function;
       Append(~L, load_node(helpfunction, s, Sprint(i)));
     end for;
  end function;
  return load_node(g, s: key := "");
end function;

forward load_type_params;
forward load_type_params_with_key;

function load_params_node(s)
  T := decode_type(s);
  f := function(x)
    return load_type_params(s, T);
  end function;
  return load_node(f, s: key:= "params");
end function;

function load_typed_object(s: override_params :="")
  T := decode_type(s);
  //TODO: This is hacked together

  if Type(s`obj) eq MonStgElt and is_uuid(s`obj) then
    return load_ref(s);
  else 
    tp := load_type_params_with_key(s, T, type_key);
  end if;

  if is_singleton(SerializationType(T)) then
    if T eq FldRat then
      return Rationals();
    elif T eq RngInt then
      return Integers();
    end if;
  else
    g := function(x)
      return load_object(s, tp);
    end function;
    return load_node(g, s: key :="data");
  end if;
end function;

function load_type_array_params(s)
  f:= function(obj)
    T:= decode_type(s);
    if Type(obj) eq MonStgElt then
      if is_uuid(s`obj) then
        return load_ref(s);
      else
        return T;
      end if;
    end if;
    return load_type_params(s, T)`params;
  end function;

  return load_array_node(f, s);
end function;

function load_type_params(s, T)
  if Type(s`obj) eq MonStgElt then
    if is_uuid(s`obj) then
      tp := SerializationType(T);
      set_params(tp, load_ref(s));
      return tp;
    else
      return SerializationType(T);
    end if;
  else
    if "params" in Keys(s`obj) then
      f := function(obj)
        if Type(obj) eq SeqEnum then
          params := load_type_array_params(s);
        elif Type(obj) eq MonStgElt or "params" in Keys(s`obj) then
          U := decode_type(s);
          if is_singleton(SerializationType(U)) then
            if T eq FldRat then
              return SerializationType(FldRat);
            elif T eq RngInt then
              return SerializationType(RngInt);
            end if;
          else
            //Hack in case were looking for the subtype of a SeqEnum that doesn't have parameters.
            if Type(T) eq Cat and T eq SeqEnum then
              params := load_type_params(s, U);
            else 
              params:= load_type_params(s, U)`params;
            end if;
          end if;
        elif not (type_key in Keys(s`obj)) then
          params:= AssociativeArray();
         
          for k -> v in obj do
            g:= function(obj)
              U := decode_type(s);
              if Type(obj) eq MonStgElt and not(is_uuid(obj)) then
              end if;
            end function;
            params[k] := load_node(g, s: key:=k);
          end for;
        else
          params := load_typed_object(s);
        end if;
        tp := SerializationType(T);
        set_params(tp, params);
        return tp;
      end function;
      return load_node(f, s: key := "params");
    else 
      tp := SerializationType(T);
      set_params(tp, load_typed_object(s));
      return tp;
    end if;
  end if;
end function;

function load_type_params_with_key(s, T, key)
  g := function(x)
    return load_type_params(s, T);
  end function;
  return load_node(g, s : key := key);
end function;

function load_typed_object_with_key(s, key :override_params := "")
  g := function(node)
    if Type(node) eq MonStgElt and is_uuid(node) then
      return load_ref(s);
    else
      return load_typed_object(s :override_params := override_params);
    end if;
  end function;
  return load_node(g, s: key := key);
end function;

function load_ref(s)
  uuid := s`obj;
  id_to_obj := get("id_to_obj");
  if uuid in Keys(id_to_obj) then
    loaded_ref := id_to_obj[uuid];
  else
    s`obj := s`refs[uuid];
    loaded_ref := load_typed_object(s);
    id_to_obj[uuid] := loaded_ref;
    set("id_to_obj", id_to_obj);
  end if;
  loaded_ref`uuid := uuid;
  return loaded_ref;
end function;


// ** Overloading load function **

load_obj_overloader := get("load_object");


// ** Basic types **

function load_bool(s)
  str := s`obj;
  if str eq "true" then
    return true;
  end if;
  if str eq "false" then
    return false;
  end if;
end function;

function load_string(s)
  return s`obj;
end function;

// ** Arrays and Lists **

function load_seq_enum_tuple(s, tp)

  T := tp`type;
  f := function(v)
    len := #v;
    if len eq 0 then
      return [];
    else
      loaded_v := [];
      for i in [1..len] do
        g := function(loaded_v)
          return load_object(s, tp);
        end function;
        Append(~loaded_v, load_node(g, s: key:=i));
      end for;
      return loaded_v;
    end if;
  end function;
  return load_node(f, s);
end function;

function load_seq_enum(s, tp)
  g:= function(v)
    if serialize_id(tp) then
      h := function(x)
        return load_ref(s);
      end function;
      return load_array_node(h, s);
    else
      loaded_v := [];
      for i in [1..#v] do
        Append(~loaded_v, load_object_with_key(s, tp, i));
      end for;
    end if;
    return loaded_v;
    end function;
    return load_node(g, s);
end function;

function load_array(s, params)
  f:= function(entries)
    len := #entries;
    if len eq 0 then
      return [];
    end if;
    tp := SerializationType(SeqEnum);
    set_params(tp, params);
    S := [load_object_with_key(s, tp, i) : i in [1..len]];
    return S;
    end function;
  return load_node(f, s);
end function;

// ** Basic ring elements **

function load_data_z_elt(s)
  f := function(str)
    return StringToInteger(str);
  end function;
  return load_node(f, s);
end function;

function load_data_q_elt(s)
  f := function(q)
    num_den := Split(q, "//");
    return StringToInteger(num_den[1])/StringToInteger(num_den[2]);
  end function;
  return load_node(f, s);
end function;

// ** Polynomial Rings **

function load_poly_ring(s, base_ring)
  tp := SerializationType(SeqEnum);
  set_params(tp, SerializationType(MonStgElt));
  symbols := load_object_with_key(s, tp, "symbols");
  R := PolynomialRing(base_ring, #symbols);
  AssignNames(~R, symbols);
  return R;
end function;

function load_univ_poly_ring(s, base_ring)
  tp := SerializationType(SeqEnum);
  set_params(tp, SerializationType(MonStgElt));
  symbols := load_object_with_key(s, tp, "symbols");
  R := PolynomialRing(base_ring);
  AssignNames(~R, symbols);
  return R;
end function;

// ** Elements of polynomial rings **

function load_univ_polynomial(s, parent_ring)
  
  f:= function(terms)
    if #terms eq 0 then
      return Zero(parent_ring);
    end if;
    
    exponents :=[];
    for i in [1..#terms] do
      g := function(x)
        return load_object_with_key(s, SerializationType(RngIntElt), 1);
      end function;
      e := load_node(g, s: key := i);
      Append(~exponents, e);
    end for;
    
    n := Maximum(exponents);
    base := BaseRing(parent_ring);
    polynomial := parent_ring!0;
    coeff_type := ElementType(base);
    
    for i in [1..#exponents] do
      g := function(x)
        c := base!0;
        f2 := function(x)
          tp := SerializationType(coeff_type);
          if serialize_params(tp) then
            params := base;
            set_params(tp, params);
          end if;
            return load_object(s, tp);
        end function;
        c := load_node(f2, s: key := 2);
        return c * parent_ring.1^exponents[i]; 
      end function;
      
      mon := load_node(g, s: key := i);
      polynomial +:= mon;
    end for;
    return polynomial;
  end function;
  return load_node(f, s);
end function;


function load_polynomial(s, parent_ring)
  g:= function(terms)
    exponents := [term[1] : term in terms];
    n := Rank(parent_ring);
    base := BaseRing(parent_ring);
    polynomial := parent_ring!0;
    coeff_type := ElementType(base);
    
    for i in [1..#exponents] do
      f := function(x)
        c := base!0;
        f2 := function(x)
          tp := SerializationType(coeff_type);
          if serialize_params(tp) then
            params := base;
            set_params(tp, params);
          end if;
          return load_object(s, tp);
        end function;
          c := load_node(f2, s: key := 2);
        e_int := [StringToInteger(x) : x in exponents[i]];
        return c * &*[parent_ring.j^e_int[j] : j in [1..n]]; 
      end function;
      mon := load_node(f, s: key := i);
      polynomial +:= mon;
    end for;
    return polynomial;
  end function;
  
  return load_node(g, s);
   
end function;

// ** Matrices **

function load_matrix_space(s, base_ring)
  ncols := load_object_with_key(s, SerializationType(RngIntElt), "ncols");
  nrows := load_object_with_key(s, SerializationType(RngIntElt), "nrows");
  return RMatrixSpace(base_ring, nrows, ncols);
end function;

function load_matrix(s, parent)
  base := BaseRing(parent);
  T := ElementType(base);
  
  tp:= SerializationType(T);
  if serialize_params(tp) then
   params := base;
   set_params(tp, params);
  end if;
  m:= load_array(s, tp);
  return parent!m;
end function;


// ** Finite fields **

function load_Fq(s, tp)
  if assigned tp`params then
  pol_tp := SerializationType(RngUPolElt);
  set_params(pol_tp,tp`params);
    def_pol := load_object(s, pol_tp);
    R := Parent(def_pol);
    F := BaseRing(R);
    a, b := ext<F |def_pol>;
    return a;
  else
    f := function(node)
      order := StringToInteger(node);
      return FiniteField(order);
    end function;
    return load_node(f, s);
  end if;
end function;

// ** Elements of finite fields **

function load_Fq_elt(s, parent)
  if IsPrimeField(parent) then
    return parent!StringToInteger(s`obj);
  end if;
  pol_ring := Parent(DefiningPolynomial(parent));
  a := parent.1;
  tp := SerializationType(RngUPolElt);
  set_params(tp, pol_ring);
  return Evaluate(load_object(s, tp), a);
end function;

// ** Number fields **
function load_simple_num_fld(s, tp)
  pol_tp := SerializationType(RngUPolElt);
  set_params(pol_tp,tp);
  def_pol := load_object_with_key(s, pol_tp, "def_pol");
  var := load_object_with_key(s, SerializationType(MonStgElt), "var");
  R := Parent(def_pol);
  F := BaseRing(R);
  K := NumberField(def_pol);
  AssignNames(~K, [var]);
  return K;
end function;

function load_simple_num_fld_elt(s, parent)
  pol_ring := Parent(DefiningPolynomial(parent));
  a := parent.1;
  tp := SerializationType(RngUPolElt);
  set_params(tp, pol_ring);
  return Evaluate(load_object(s, tp), a);
end function;

function load_order(s, base_ring)
  tp := SerializationType(SeqEnum);
  coeff_tp:=SerializationType(ElementType(base_ring));
  set_params(coeff_tp, base_ring);
  set_params(tp, coeff_tp);
  basis := load_object(s, tp);
  OK := Order(basis:Verify:=false, IsBasis:=true);
  return OK;
end function;

function load_order_elt(s, parent)
  tp := SerializationType(SeqEnum);
  coeff_tp := SerializationType(ElementType(FieldOfFractions(BaseRing(parent))));
  set_params(tp, coeff_tp);
  return parent!load_object(s, tp);
end function;


// ** Real fields **
function load_real_fld(s)
  f := function(node)
  prec := StringToInteger(node);
    return RealField(prec);
  end function;
  return load_node(f, s);
end function;

//This does not necessarily give equality in a round trip, but does load exactly what is given in the file. 
// (Works well enough for now. For equality, might need to change saving)
function load_real_fld_elt(s, parent)
  value := load_object(s, SerializationType(MonStgElt));
  decompose := Split(value, "E");
  if #decompose eq 2 then
    exp:= StringToInteger(decompose[2]);
  else
    exp:=0;
  end if;
  value:= decompose[1];
  value:= Split(value, ".");
  n := #value[2];
  num, den := Explode([StringToInteger(v) : v in value]);
  RR := parent;
  return RR!((num + (den * 10^(-n)))*10^exp);
end function;

// ** Complex fields **
function load_com_fld(s)
  prec := load_object_with_key(s, SerializationType(RngIntElt), "prec");
  var := load_object_with_key(s, SerializationType(MonStgElt), "var");
  CC:= ComplexField(prec);
  AssignNames(~CC, [var]);
  return CC;
end function;

function load_com_fld_elt(s, parent)
  tp_seq_enum := SerializationType(SeqEnum);
  tp_re := SerializationType(FldReElt);
  set_params(tp_re, parent);
  set_params(tp_seq_enum, tp_re);
  real_part, imag_part := Explode(load_object(s, tp_seq_enum));
  
  return real_part + imag_part*parent.1;
end function;

//Overloading the load function 
//Note: Ring of integers and Rationals are treated separately in load_typed_object as they are singleton types.
load_obj_overloader[MonStgElt] :=load_string;
load_obj_overloader[BoolElt] := load_bool;
load_obj_overloader[RngIntElt] := load_data_z_elt;
load_obj_overloader[FldRatElt] := load_data_q_elt;
load_obj_overloader[FldRe] := load_real_fld;
load_obj_overloader[FldReElt] := load_real_fld_elt;
load_obj_overloader[FldCom] := load_com_fld;
load_obj_overloader[FldComElt] := load_com_fld_elt;
load_obj_overloader[FldFin] := load_Fq;
load_obj_overloader[FldFinElt] := load_Fq_elt;
load_obj_overloader[SeqEnum] := load_seq_enum;
load_obj_overloader[RngMPol] := load_poly_ring;
load_obj_overloader[RngUPol] := load_univ_poly_ring;
load_obj_overloader[RngUPolElt] := load_univ_polynomial;
load_obj_overloader[RngMPolElt] := load_polynomial;
load_obj_overloader[ModMatRng] := load_matrix_space;
load_obj_overloader[ModMatRngElt] := load_matrix;
load_obj_overloader[AlgMat] := load_matrix_space;
load_obj_overloader[AlgMatElt] := load_matrix;
load_obj_overloader[FldNum] := load_simple_num_fld;
load_obj_overloader[FldNumElt] := load_simple_num_fld_elt;
load_obj_overloader[RngOrd] := load_order;
load_obj_overloader[RngOrdElt] := load_order_elt;

set("load_object", load_obj_overloader);

// Define load function

intrinsic load_mardi_json(path::MonStgElt) -> Any
{}
  file := Open(path, "r");
  s := deserializer_open(file);
  loaded := load_typed_object(s);
  return loaded;
end intrinsic;

