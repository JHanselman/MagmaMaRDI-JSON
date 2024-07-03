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

//Serialization info

function serialization_version_info(obj)
  ns := obj["_ns"];
  version_info := ns["Oscar"];
  return version_info;
end function;


function get_serialization_version()
A := AssociativeArray();
A["Oscar"] :=  ["https://github.com/oscar-system/Oscar.jl", "1.0.2"];
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


procedure save_object(s, x)
  T := Type(x);
  f:= get("save_object")[T];
  f(s,x);
end procedure;

procedure save_type_params(s, x)
  T := Type(x);
  f:= get("save_type_params")[T];
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


procedure save_type_params(s, obj)
  T := Type(obj);
  get("save_type_params")[T](s, obj);
end procedure;

procedure save_type_params_with_key(s, obj, key)
  set_key(s, key);
  save_type_params(s, obj);
end procedure;

function save_as_ref(s, obj) 
  serialize_id := get("serialize_id");
  T:= Type(obj);
  if serialize_id[T] then
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
  encode_type := get("encode_type");
  serialize_params := get("serialize_params");
  is_singleton := get("is_singleton");
  T:=Type(x);
  if serialize_params[T] then
    save_type_params_with_key(s, x, type_key);
    save_object_with_key(s, x, "data");
  elif is_singleton[T] then
    save_object_with_key(s, encode_type[T], type_key);
  else
    save_object_with_key(s, encode_type[T], type_key);
    save_object_with_key(s, x, "data");
  end if;
end procedure;

procedure save_typed_object_with_key(s, x, key)
  serialize_id := get("serialize_id");
  if not (key eq "") then
    set_key(s, key);
    T := Type(x);
    if serialize_id[T] then
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
  serialize_id := get("serialize_id");
  begin_array_node(s);
    for elem in x do
      if serialize_id[Type(elem)] then
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
    save_typed_object_with_key(s, BaseRing(obj), "base_ring");
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
    save_typed_object_with_key(s, BaseRing(obj), "base_ring");
    M := Zero(obj);
    save_object_with_key(s, Ncols(M), "ncols");
    save_object_with_key(s, Nrows(M), "nrows");
  end_dict_node(s);
end procedure;

procedure save_alg_mat(s, obj)
   begin_dict_node(s);
    save_typed_object_with_key(s, BaseRing(obj), "base_ring");
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
    begin_dict_node(s);
      save_typed_object(s, DefiningPolynomial(obj));
    end_dict_node(s);
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


//Overloading the save function 

save_obj_overloader[MonStgElt] := save_data_basic;
save_obj_overloader[BoolElt] := save_data_basic;
save_obj_overloader[RngIntElt] := save_data_basic;
save_obj_overloader[FldRatElt] := save_data_q_elt;
save_obj_overloader[SeqEnum] := save_data_seq_enum;
save_obj_overloader[Assoc] := save_data_assoc;
save_obj_overloader[RngMPol] := save_poly_ring;
save_obj_overloader[RngUPol] := save_poly_ring;
save_obj_overloader[RngMPolElt] := save_mpoly_elt;
save_obj_overloader[RngUPolElt] := save_upoly_elt;
save_obj_overloader[FldFin] := save_Fq;
save_obj_overloader[FldFinElt] := save_Fq_elt;
save_obj_overloader[ModMatFld] := save_mat_space;
save_obj_overloader[ModMatRng] := save_mat_space;
save_obj_overloader[AlgMat] := save_alg_mat;
save_obj_overloader[ModMatFldElt] := save_mtrx;
save_obj_overloader[ModMatRngElt] := save_mtrx;
save_obj_overloader[Mtrx] := save_mtrx;

set("save_object", save_obj_overloader);

//Overloading saving with parameters

procedure save_type_param_ringmat_elt(s, obj)
  T := Type(obj);
  encode_type := get("encode_type");
  serialize_id := get("serialize_id");
  begin_dict_node(s);
    save_object_with_key(s, encode_type[T], "name");
    parent_obj := Parent(obj);
    if serialize_id[Type(parent_obj)] then
      parent_ref := save_as_ref(s, parent_obj);
      save_object_with_key(s, parent_ref, "params");
    else
      save_typed_object_with_key(s, parent_obj, "params");
    end if;
  end_dict_node(s);
end procedure;

//Might have problems with empty seqenum

procedure save_type_param_seqenum(s, obj)
  T := Type(obj);
  encode_type := get("encode_type");
  serialize_params := get("serialize_params");
  begin_dict_node(s);
    save_object_with_key(s, encode_type[T], "name");
    
    if obj ne [] and serialize_params[Type(obj[1])] then
      save_type_params_with_key(s, obj[1], "params");
    else
      save_object_with_key(s, encode_type[Type(obj[1])], "params");
    end if;
  end_dict_node(s);
end procedure;

save_type_params_overloader[RngMPolElt] := save_type_param_ringmat_elt;
save_type_params_overloader[RngUPolElt] := save_type_param_ringmat_elt;
save_type_params_overloader[FldFinElt] := save_type_param_ringmat_elt;
save_type_params_overloader[ModMatFldElt] := save_type_param_ringmat_elt;
save_type_params_overloader[ModMatRngElt] := save_type_param_ringmat_elt;
save_type_params_overloader[Mtrx] := save_type_param_ringmat_elt;
save_type_params_overloader[SeqEnum] := save_type_param_seqenum;

set("save_type_params", save_type_params_overloader);

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
    T := Type(obj);
    serialize_id := get("serialize_id");
    
    //Store the id if serialize_id is true
    if serialize_id[T] then
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


