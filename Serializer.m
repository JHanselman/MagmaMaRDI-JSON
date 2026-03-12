import "Globals.m": JSON_COMMA, JSON_COLON, JSON_LEFTBRACKET, JSON_RIGHTBRACKET, JSON_LEFTBRACE, JSON_RIGHTBRACE, JSON_QUOTE, JSON_WHITESPACE, JSON_SYNTAX, FALSE_LEN, TRUE_LEN, NULL_LEN;

import "Globals.m": get, set, type_key, refs_key;

//Define Serializer object

declare type SerState;
declare attributes SerState: new_level_entry, refs, io, key;

intrinsic Print(X::SerState)
{Print X}
  printf("SerializerState for JSON object");
end intrinsic;

intrinsic SerializerState(new_level::BoolElt, uuids::SeqEnum, io::IO, key::MonStgElt) -> SerState
{Initialize serializer state}
  S := New(SerState);
  S`new_level_entry := new_level;
  S`refs := uuids;
  S`io := io;
  S`key := key;
  return S;
end intrinsic;


declare type SerCat;
declare attributes SerCat: type, encoding, serialize_params, serialize_with_id, is_singleton, params;

intrinsic SerializationType(T:Cat) -> SerCat
{Initialize SerCat}
  ST := New(SerCat);
  ST`type := T;
  type_data := get("type_data")[T];

  ST`serialize_params := type_data[2];
  ST`serialize_with_id := type_data[3];
  ST`is_singleton := type_data[4];

  return ST;
end intrinsic;

intrinsic Print(X::SerCat)
{Print X}
if assigned X`params then
  print "Serialization Type", X`type,",",X`params;
else
  print "Serialization Type", X`type, ", undefined";
end if;
end intrinsic;


intrinsic SerializationType(obj::Any) -> SerCat
{Initialize SerCat}

  ST := New(SerCat);
  T:= Type(obj);
  if Type(obj) eq Cat then
    T:= obj;
    type_data := get("type_data")[T];
  else
    type_data := get("type_data")[T];
    if Type(type_data[1]) eq MonStgElt then
      ST`encoding := type_data[1];
      else
      f := type_data[5];
      ST`encoding := type_data[1][f(obj)];
    end if;
  end if;

  
  ST`type := T;
  ST`serialize_params := type_data[2];
  ST`serialize_with_id := type_data[3];
  ST`is_singleton := type_data[4];

  return ST;
end intrinsic;



procedure set_params(T, S)
  T`params := S;
end procedure;

//Serialization info

function serialization_version_info(obj)
  ns := obj["_ns"];
  version_info := ns["Oscar"];
  return version_info;
end function;


function get_serialization_version()
A := AssociativeArray();
A["Oscar"] :=  ["https://github.com/oscar-system/Oscar.jl", "1.7.0"];
return A;
end function;


// Generic saving functions

procedure begin_node(s:SerState)
  if (not s`new_level_entry) then
    Write(s`io, JSON_COMMA);
  else
    s`new_level_entry := false;
  end if;
  //Empty string means key is not set. 
  if (s`key ne "") then
    key := s`key;
    Write(s`io, JSON_QUOTE cat key cat JSON_QUOTE cat JSON_COLON);
    s`key:= "";
  end if;
end procedure;
    
procedure begin_dict_node(s)
  begin_node(s);
  Write(s`io, JSON_LEFTBRACE);
  s`new_level_entry := true;
end procedure;

procedure end_dict_node(s)
  Write(s`io, JSON_RIGHTBRACE);
end procedure;

procedure begin_array_node(s)
  begin_node(s);
  Write(s`io, JSON_LEFTBRACKET);
  s`new_level_entry := true;
end procedure;

procedure end_array_node(s)
  Write(s`io, JSON_RIGHTBRACKET);
  if s`new_level_entry then
    // makes sure that entries after empty arrays add comma
    s`new_level_entry := false;
  end if;
end procedure;

procedure set_key(s, key)
  //@req thing?
  s`key := key;
end procedure;

function encode_type(x)
  return x`encoding;
end function;

function serialize_params(x)
  return x`serialize_params;
end function;

function serialize_id(x)
  return x`serialize_with_id;
end function;

function is_singleton(x)
  return x`is_singleton;
end function;

function type_params(obj)
  T := SerializationType(obj);
  encoding := encode_type(T);
  base_encoding:= Sprint(Type(obj));
  type_param := get("type_params");
  //We check if there is a special way to handle type params based on the encoding.
  //If not we use the default way specified by the type.
  if encoding in Keys(type_param) then
    return type_param[encoding](obj);
  elif base_encoding in Keys(type_param) then
    return type_param[base_encoding](obj);
  else
    return T;
  end if;
end function;

function type_param_ringmat(obj)
  T := SerializationType(obj);
  S := BaseRing(obj);
  set_params(T, S);
  return T;
end function;

function type_param_ringmat_elt(obj)
  T := SerializationType(obj);
  S := Parent(obj);
  set_params(T, S);
  return T;
end function;

function type_param_FF(obj)
  T := SerializationType(obj);
  if Degree(obj) eq 1 then
    return T;
  else 
    S := Parent(DefiningPolynomial((obj)));
    set_params(T, S);
    return T;
  end if;
end function;

function type_param_num_field(obj)
  T := SerializationType(obj);
  S := Parent(DefiningPolynomial((obj)));
  set_params(T, S);
  return T;
end function;

function type_param_order(obj)
  T := SerializationType(obj);
  S := NumberField(obj);
  set_params(T, S);
  return T;
end function;

function type_param_ideal(obj)
  T := SerializationType(obj);
  S := Order(obj);
  set_params(T, S);
  return T;
end function;


//Might have problems with empty seqenum

function type_param_seqenum(obj)
  T := SerializationType(obj);
  S := type_params(obj[1]);
  set_params(T, S);
  return T;
end function;

type_params_overloader := get("type_params");
type_params_overloader["RngUPol"] := type_param_ringmat;
type_params_overloader["RngMPol"] := type_param_ringmat;
type_params_overloader["ModMatFld"] := type_param_ringmat;
type_params_overloader["ModMatRng"] :=  type_param_ringmat;
type_params_overloader["ModMatFld"] := type_param_ringmat;
type_params_overloader["RngMPolElt"] := type_param_ringmat_elt;
type_params_overloader["RngUPolElt"] := type_param_ringmat_elt;
type_params_overloader["FldFinElt"] := type_param_ringmat_elt;
type_params_overloader["ModMatFldElt"] := type_param_ringmat_elt;
type_params_overloader["ModMatRngElt"] := type_param_ringmat_elt;
type_params_overloader["FldFinElt"] := type_param_ringmat_elt;
type_params_overloader["Mtrx"] := type_param_ringmat_elt;
type_params_overloader["AlgMatElt"] := type_param_ringmat_elt;
type_params_overloader["SeqEnum"] := type_param_seqenum;
type_params_overloader["FldFin"] := type_param_FF;
type_params_overloader["FldNum"] := type_param_num_field;
type_params_overloader["RngOrd"] := type_param_order;
type_params_overloader["RngOrdElt"] := type_param_ringmat_elt;
type_params_overloader["RngOrdIdl"] := type_param_ideal;
type_params_overloader["FldNumElt"] := type_param_ringmat_elt;
type_params_overloader["FldReElt"] := type_param_ringmat_elt;
type_params_overloader["FldComElt"] := type_param_ringmat_elt;
set("type_params", type_params_overloader);


procedure save_object(s, x)
  save_object_overloader := get("save_object");
  T := SerializationType(x);
  encoding := encode_type(T);
  base_encoding := Sprint(Type(x));
  if encoding in Keys(save_object_overloader) then
    f:= save_object_overloader[encoding];
  elif base_encoding in Keys(save_object_overloader) then
    f:= save_object_overloader[base_encoding];
  else
    error("Saving this type is not yet supported");
  end if;
  f(s,x);
end procedure;


procedure save_object_with_key(s , obj, key)
  set_key(s, key);
  save_object(s, obj);
end procedure;

procedure save_header(s, dict, key)
  if not (key eq "") then
    set_key(s, key);
  end if;
  begin_dict_node(s);
  for k -> v in dict do
    save_object_with_key(s, v, k);
  end for;
  end_dict_node(s);
end procedure;

forward save_type_params_with_key;
forward save_typed_object_with_key;

//Case where S is nothing.
procedure save_type_params(s, T)
  if not assigned T`params then
    save_object(s, encode_type(T));
  else
     begin_dict_node(s);
        save_object_with_key(s, encode_type(T), "name");
        params := T`params;
        if Type(params) eq SerCat then
          save_type_params_with_key(s, params, "params");
        elif Type(params) eq Assoc then
          error "Not implemented yet.";
        else
          save_typed_object_with_key(s, params, "params");
        end if;
     end_dict_node(s);
  end if;
end procedure;

procedure save_type_params_with_key(s, obj, key)
  set_key(s, key);
  save_type_params(s, obj);
end procedure;

function save_as_ref(s, obj) 
  T:= SerializationType(obj);
  if serialize_id(T) then
    if assigned obj`uuid then
      ref := obj`uuid;
      if ref notin s`refs then
        Append(~s`refs, ref);
      end if;
    return ref;
    else
      ref:= uuid4();
      obj`uuid := ref;
      id_to_obj := get("id_to_obj");
      id_to_obj[ref] := obj;
      set("id_to_obj", id_to_obj);
      Append(~s`refs, ref);
      return ref;
    end if;
  else
    error("Can't assign uuid to object of type not Cat");
  end if;
end function;

procedure save_typed_object(s, x)
  T := SerializationType(x);
  if is_singleton(T) then
    save_object_with_key(s, encode_type(T), type_key);
  else
    save_type_params_with_key(s, type_params(x), type_key);
    save_object_with_key(s, x, "data");
  end if;
end procedure;

procedure save_typed_object_with_key(s, x, key)
  if not (key eq "") then
    set_key(s, key);
    T := SerializationType(x);
    if serialize_id(T) then
      ref := save_as_ref(s, x);
      save_object(s, ref);
    else
      begin_dict_node(s);
        save_typed_object(s, x);
      end_dict_node(s);
    end if;
  end if;
end procedure;


// Overloading save function

save_obj_overloader := get("save_object");
save_type_params_overloader := get("save_type_params");

// ** Basic types **

procedure save_data_basic(s, x)
  begin_node(s);
  str := JSON_QUOTE cat Sprint(x) cat JSON_QUOTE;
  Write (s`io, str);
end procedure;

// ** Arrays and Lists **

procedure save_data_seq_enum(s, x)
  begin_array_node(s);
    for elem in x do
      if serialize_id(SerializationType(elem)) then
        ref := save_as_ref(s, elem);
        save_object(s, ref);
      else
        save_object(s, elem);
      end if;
    end for;
  end_array_node(s);
end procedure;

procedure save_data_assoc(s, x)
  begin_array_node(s);
    for k -> v in x do
      save_object(s, [k,v]);
    end for;
  end_array_node(s);
end procedure;

// ** Basic ring elements **

procedure save_data_q_elt(s, x)
  num := Numerator(x);
  den := Denominator(x);
  begin_node(s);
  str := JSON_QUOTE cat Sprint(num) cat "//" cat Sprint(den) cat JSON_QUOTE;
  Write (s`io, str);
end procedure;

// ** Polynomial Rings **

procedure save_poly_ring(s, obj)
  begin_dict_node(s);
  save_object_with_key(s, Names(obj), "symbols"); 
  end_dict_node(s);
end procedure;

// ** Elements of polynomial rings **

procedure save_mpoly_elt(s, p)
  //Is this good for anything? TODO: How to save 0 polynomial
  // we use this line instead of typeof(coeff(p, 1)) to catch the 0 polynomial
  //coeff_type := Type(One(BaseRing(Parent(p))));
  coeffs, mons := CoefficientsAndMonomials(p);
  exp_vectors := [Exponents(mon) : mon in mons];
  begin_array_node(s);
    for i in [1..#coeffs] do
      begin_array_node(s);
        save_object(s, [Sprint(e) : e in exp_vectors[i]]);
        save_object(s, coeffs[i]);
      end_array_node(s);
    end for;
  end_array_node(s);
end procedure;

procedure save_upoly_elt(s, p)
  begin_array_node(s);
    for t in Terms(p) do
      begin_array_node(s);
        e := Valuation(t);
        save_object(s, Sprint(e));
        save_object(s, Coefficient(t, e));
      end_array_node(s);
    end for;
  end_array_node(s);
end procedure;

// ** Matrices **

procedure save_mat_space(s, obj)
  begin_dict_node(s);
    M := Zero(obj);
    save_object_with_key(s, Ncols(M), "ncols");
    save_object_with_key(s, Nrows(M), "nrows");
  end_dict_node(s);
end procedure;

procedure save_alg_mat(s, obj)
   begin_dict_node(s);
    save_object_with_key(s, Degree(obj), "ncols");
    save_object_with_key(s, Degree(obj), "nrows");
  end_dict_node(s);
end procedure;

procedure save_mtrx(s, obj)
    save_object(s, RowSequence(obj));
end procedure;

// ** Finite fields **

procedure save_Fq(s, obj)
  if Degree(obj) eq 1 then
    save_object(s, Sprint(Characteristic(obj)));
  else
      save_object(s, DefiningPolynomial(obj));
  end if;
end procedure;

// ** Elements of finite fields **

procedure save_Fq_elt(s, obj)
  K := Parent(obj);
  if Degree(K) eq 1 then
    save_data_basic(s, obj);
  else
    poly_parent := BaseRing(Parent(DefiningPolynomial(K)));
    polynomial := Polynomial(poly_parent, Eltseq(obj));
    save_object(s, polynomial);
  end if;
end procedure;

// ** Number fields **

procedure save_num_fld(s, obj)
  if not IsSimple(obj) then
    error("Cannot save number fields that are non-simple yet.");
  end if;
    begin_dict_node(s);
      save_object_with_key(s, DefiningPolynomial(obj), "def_pol");
      save_object_with_key(s, Sprint(Name(obj,1)), "var");
    end_dict_node(s);
end procedure;

procedure save_num_fld_elt(s, obj)
  K := Parent(obj);
  if not IsSimple(K) then
    error("Cannot save elements of number fields that are non-simple yet.");
  end if;
  R := Parent(DefiningPolynomial(K));
  polynomial := R!(Eltseq(obj));
  save_object(s, polynomial);
end procedure;


//The methods below do not necessarily match up with Oscar.

procedure save_order(s, obj)
  K:= NumberField(obj);
  save_object(s, Basis(obj, K));
end procedure;

procedure save_order_elt(s, obj)
  save_object(s, Eltseq(obj));
end procedure;

procedure save_ideal(s, obj)
  save_object(s, Basis(obj));
end procedure;

procedure save_real_fld(s, obj)
      save_object(s, Precision(obj));
end procedure;

procedure save_real_fld_elt(s, obj)
  save_object(s, Sprint(obj));
end procedure;

procedure save_com_fld(s, obj)
    begin_dict_node(s);
      save_object_with_key(s, Precision(obj), "prec");

      //Convoluted way of getting the variable name. It should be possible to do this easier right?
      var_name:= Split(Sprint(obj.1),"*");
      Remove(~var_name, 1);
      var_name := &cat(var_name);

      save_object_with_key(s, var_name, "var");
    end_dict_node(s);
end procedure;

procedure save_com_fld_elt(s, obj)
  save_object(s, [Real(obj), Imaginary(obj)]);
end procedure;

// ** Elliptic Curves **
//Probably works. Is untested.
/*procedure save_elliptic_curve(s, obj)
  begin_dict_node(s);
    save_typed_object_with_key(s, BaseRing(obj), "base_ring");
    save_object_with_key(s, aInvariants(obj), "a_invariants"); 
  end_dict_node(s);
end procedure;
*/

//Overloading the save function 

//Basics
save_obj_overloader["MonStgElt"] := save_data_basic;
save_obj_overloader["BoolElt"] := save_data_basic;

//Lists
save_obj_overloader["SeqEnum"] := save_data_seq_enum;
save_obj_overloader["Assoc"] := save_data_assoc;

//Basis numbers
save_obj_overloader["RngIntElt"] := save_data_basic;
save_obj_overloader["FldRatElt"] := save_data_q_elt;


//Polynomials
save_obj_overloader["RngMPol"] := save_poly_ring;
save_obj_overloader["RngUPol"] := save_poly_ring;
save_obj_overloader["RngMPolElt"] := save_mpoly_elt;
save_obj_overloader["RngUPolElt"] := save_upoly_elt;

//Finite Fields
save_obj_overloader["FldFin"] := save_Fq;
save_obj_overloader["FldFinElt"] := save_Fq_elt;

//Matrices
save_obj_overloader["ModMatFld"] := save_mat_space;
save_obj_overloader["ModMatRng"] := save_mat_space;
save_obj_overloader["AlgMat"] := save_alg_mat;
save_obj_overloader["ModMatFldElt"] := save_mtrx;
save_obj_overloader["ModMatRngElt"] := save_mtrx;
save_obj_overloader["AlgMatElt"] := save_mtrx;
save_obj_overloader["Mtrx"] := save_mtrx;

//Number fields
save_obj_overloader["FldNum"] := save_num_fld;
save_obj_overloader["FldNumElt"] := save_num_fld_elt;
save_obj_overloader["RngOrd"] := save_order;
save_obj_overloader["RngOrdElt"] := save_order_elt;
save_obj_overloader["RngOrdIdl"] := save_ideal;

//Real and Complex Fields
save_obj_overloader["FldRe"] := save_real_fld;
save_obj_overloader["FldReElt"] := save_real_fld_elt;
save_obj_overloader["FldCom"] := save_com_fld;
save_obj_overloader["FldComElt"] := save_com_fld_elt;

//Curves
//save_obj_overloader[CrvEll] := save_elliptic_curve;

set("save_object", save_obj_overloader);


// Define save function

intrinsic save_mardi_json(file::MonStgElt, obj::Any)
{}
  io := Open(file, "w");
  s:= SerializerState(true, [], io, "");
  begin_dict_node(s);
    //Write the header
    save_header(s, get_serialization_version(), "_ns");
    
    //Save the object
    save_typed_object(s, obj);
    T := SerializationType(obj);
    
    //Store the id if serialize_id is true
    if serialize_id(T) then
      if assigned obj`uuid then
        ref := obj`uuid;
      else
        ref:= uuid4();
        obj`uuid := ref;
        id_to_obj := get("id_to_obj");
        id_to_obj[ref] := obj;
        set("id_to_obj", id_to_obj);
      end if;
      save_object_with_key(s, ref, "id");
    end if;
    if #s`refs ne 0 then
      set_key(s, refs_key);
      begin_dict_node(s);
      refs_done :=[];
        while #s`refs ne 0 do
          ref_id := s`refs[1];
          id_to_obj := get("id_to_obj");
          ref_obj := id_to_obj[ref_id];
          set_key(s, ref_id);
          begin_dict_node(s);
            save_typed_object(s, ref_obj);
          end_dict_node(s);
          Remove(~s`refs, 1);
        end while;
      end_dict_node(s);
    end if;
  end_dict_node(s);
end intrinsic;


