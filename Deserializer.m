import "Globals.m":type_key, refs_key, get, set; 
import "Serializer.m":set_key; 

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

function load_object(s, T)
  f:= get("load_object")[T];
  return f(s);
end function;

function load_object_with_params(s, T, params)
  f:= get("load_object")[T];
  return f(s, params);
end function;

function load_object_with_key(s, T, key)
  g := function(x)
    return load_object(s, T);
  end function;
  return load_node(g, s : key := key);
end function;

function load_object_with_params_and_key(s, T, params, key)
  g:= function(x)
    return load_object_with_params(s, T, params);
  end function;
  return load_node(g, s: key := key);
end function;

function load_type_params(s, T)
  f:= get("load_type_params")[T];
  return f(s);
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
  if get("is_singleton")[T] then
    if T eq FldRat then
      return Rationals();
    elif T eq RngInt then
      return Integers();
    end if;
  elif get("serialize_params")[T] then
    f := function(x)
      return load_params_node(s);
    end function;
    params := load_node(f, s : key := type_key); 
    g := function(x)
      return load_object_with_params(s, T, params);
    end function;
    return load_node(g, s: key :="data");
  else
    g := function(x)
      return load_object(s, T);
    end function;
    return load_node(g, s: key :="data");
  end if;
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
load_type_params_overloader := get("load_type_params");

// Helper function for objects with lots of parents
function get_parents(parent_ring)
  serialize_with_id := get("serialize_id");
  if not serialize_with_id[Type(parent_ring)] or Type(parent_ring) eq FldFin then
    return [* *];
  end if;

  base := BaseRing(parent_ring);
  parents := get_parents(base);
  Append(~parents, parent_ring);
  return parents;
end function;

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

function load_seq_enum_tuple(s, params)

  T := params[1];
  f := function(v)
    len := #v;
    if len eq 0 then
      return [];
    else
      loaded_v := [];
      for i in [1..len] do
        g := function(loaded_v)
          return load_object_with_params(s, T, params[2]);
        end function;
        Append(~loaded_v, load_node(g, s: key:=i));
      end for;
      return loaded_v;
    end if;
  end function;
  return load_node(f, s);
end function;

function load_seq_enum(s, params)
  if Type(params) ne Cat then
    if #params eq 2 then
      return load_seq_enum_tuple(s, params);
    else
      Error("parameters for SeqEnum not a type or a tuple.");
    end if;
  end if;
  T:= params;
  serialize_id := get("serialize_id");
  g:= function(v)
    if serialize_id[T] then
      h := function(x)
        return load_ref(s);
      end function;
      return load_array_node(h, s);
    else
      loaded_v := [];
      for i in [1..#v] do
        Append(~loaded_v, load_object_with_key(s, params, i));
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
    S := [load_object_with_params_and_key(s, SeqEnum, params, i) : i in [1..len]];
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

function load_type_params_ring_mat_elt(s)
  return load_typed_object(s);
end function;

function load_poly_ring(s)
  base_ring := load_typed_object_with_key(s, "base_ring");
  symbols := load_object_with_params_and_key(s, SeqEnum, MonStgElt, "symbols");
  R := PolynomialRing(base_ring, #symbols);
  AssignNames(~R, symbols);
  return R;
end function;

function load_univ_poly_ring(s)
  base_ring := load_typed_object_with_key(s, "base_ring");
  symbols := load_object_with_params_and_key(s, SeqEnum, MonStgElt, "symbols");
  R := PolynomialRing(base_ring);
  AssignNames(~R, symbols);
  return R;
end function;

// ** Elements of polynomial rings **

function load_univ_polynomial_help(s, parents)
  serialize_with_params := get("serialize_params");
  parent_ring := parents[#parents];
  
  f:= function(terms)
    if #terms eq 0 then
      return Zero(parent_ring);
    end if;
    
    exponents :=[];
    for i in [1..#terms] do
      g := function(x)
        return load_object_with_key(s, RngIntElt, 1);
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
        if serialize_with_params[coeff_type] then
          if #parents eq 1 then
            params := base;
          else
            params := parents[1..#parents-1];
          end if;
        
          f2 := function(x)
            return load_object_with_params(s, coeff_type, params);
          end function;
          c := load_node(f2, s: key := 2);
        else
          f2 := function(x)
            return load_object(s, coeff_type);
          end function;
          c := load_node(f2, s: key := 2);
        end if;
        return c * parent_ring.1^exponents[i]; 
      end function;
      
      mon := load_node(g, s: key := i);
      polynomial +:= mon;
    end for;
    return polynomial;
  end function;
  return load_node(f, s);
end function;

function load_univ_polynomial(s, parent_rings)
  if Type(parent_rings) ne List then 
    parent_rings := get_parents(parent_rings);
  end if;
  return load_univ_polynomial_help(s, parent_rings);
end function;

function load_polynomial_help(s, parents)
  serialize_with_params := get("serialize_params");
  
  g:= function(terms)
    exponents := [term[1] : term in terms];
    parent_ring := parents[#parents];
    n := Rank(parent_ring);
    base := BaseRing(parent_ring);
    polynomial := parent_ring!0;
    coeff_type := ElementType(base);
    
    for i in [1..#exponents] do
      f := function(x)
        c := base!0;
        if serialize_with_params[coeff_type] then
          if #parents eq 1 then
            params := base;
          else
            params := parents[1..#parents-1];
          end if;
        
          f2 := function(x)
            return load_object_with_params(s, coeff_type, params);
          end function;
          c := load_node(f2, s: key := 2);
        else
          f2 := function(x)
            return load_object(s, coeff_type);
          end function;
          c := load_node(f2, s: key := 2);
        end if;
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

function load_polynomial(s, parent_rings)
  if Type(parent_rings) ne List then 
    parent_rings := get_parents(parent_rings);
  end if;
  return load_polynomial_help(s, parent_rings);
end function;

// ** Matrices **

function load_matrix_space(s)
  base_ring := load_typed_object_with_key(s, "base_ring");
  ncols := load_object_with_key(s, RngIntElt, "ncols");
  nrows := load_object_with_key(s, RngIntElt, "nrows");
  return RMatrixSpace(base_ring, nrows, ncols);
end function;

function load_matrix_help(s, parents)
  parent := parents[#parents];
  base := BaseRing(parent);
  T := ElementType(base);
  serialize_with_params := get("serialize_params");
  
  if serialize_with_params[T] then
    if #parents eq 1 then
      params := base;
    else
      params := parents[1..#parents-1];
    end if;
   m := load_array(s, [* T, params *]);
  else
    m:= load_array(s, T);
  end if;
  return parent!m;
end function;

function load_matrix(s, parent_ring)
  parents := get_parents(parent_ring);
  return load_matrix_help(s, parents);
end function;

// ** Finite fields **

function load_Fq(s)
    f := function(node)
    if Type(node) eq MonStgElt then
      order := StringToInteger(node);
      return FiniteField(order);
    else
      def_pol := load_typed_object(s);
      R := Parent(def_pol);
      F := BaseRing(R);
      a, b := ext<F |def_pol>;
      return a;
    end if;
  end function;
  return load_node(f, s);
end function;

// ** Elements of finite fields **

function load_Fq_elt(s, parents)
  if Type(parents) eq List then 
    n := #parents;
    K := parents[n];
    f:= function(x)
      return K!(load_object_with_params(s, RngUPolElt, parents[1..n-1]));
    end function;
  elif Type(parents) eq FldFin and IsPrimeField(parents) then
    f := function(str)
      return parents!StringToInteger(str);
    end function;
    return load_node(f, s);
  else
    pol_ring := Parent(DefiningPolynomial(parents));
    a := parents.1;
    return Evaluate(load_object_with_params(s, RngUPolElt, pol_ring), a);
  end if;
end function;


//Overloading the load function 
//Note: Ring of integers and Rationals are treated separately in load_typed_object as they are singleton types.
load_obj_overloader[MonStgElt] :=load_string;
load_obj_overloader[BoolElt] := load_bool;
load_obj_overloader[RngIntElt] := load_data_z_elt;
load_obj_overloader[FldRatElt] := load_data_q_elt;
load_obj_overloader[FldFin] := load_Fq;
load_obj_overloader[FldFinElt] := load_Fq_elt;
load_obj_overloader[SeqEnum] := load_seq_enum;
load_obj_overloader[RngMPol] := load_poly_ring;
load_obj_overloader[RngUPol] := load_univ_poly_ring;
load_obj_overloader[RngUPolElt] := load_univ_polynomial;
load_obj_overloader[RngMPolElt] := load_polynomial;
load_obj_overloader[ModMatRng] := load_matrix_space;
load_obj_overloader[ModMatRngElt] := load_matrix;

load_type_params_overloader[RngMPolElt] := load_type_params_ring_mat_elt;
load_type_params_overloader[RngUPolElt] := load_type_params_ring_mat_elt;
load_type_params_overloader[FldFinElt] := load_type_params_ring_mat_elt;
load_type_params_overloader[ModMatRngElt] := load_type_params_ring_mat_elt;

set("load_object", load_obj_overloader);
set("load_type_params", load_type_params_overloader);

// Define load function

intrinsic load_mardi_json(path::MonStgElt) -> Any
{}
  file := Open(path, "r");
  s := deserializer_open(file);
  serialize_with_params := get("serialize_params");
  
  f := function(x)
    return decode_type(s);
  end function;
  
  T := load_node(f, s: key := type_key);
  if serialize_with_params[T] then
    f := function(x)
      return load_params_node(s);
    end function;
    params := load_node(f, s: key:= type_key);
    
    f := function(x)
      return load_object_with_params(s, T, params);
    end function;
    loaded := load_node(f, s: key := "data");
  else 
    loaded := load_typed_object(s);
  end if;
  return loaded;
end intrinsic;

