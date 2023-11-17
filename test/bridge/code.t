  $ cat target.ml
  [@@@ocaml.ppx.context
    { tool_name = "ppx_driver"
    ; include_dirs = []
    ; load_path = []
    ; open_modules = []
    ; for_package = None
    ; debug = false
    ; use_threads = false
    ; use_vmthreads = false
    ; recursive_types = false
    ; principal = false
    ; transparent_modules = false
    ; unboxed_types = false
    ; unsafe_string = false
    ; cookies = [ "library-name", "Samples" ]
    }]
  
  open GT
  open OCanren
  open OCanren.Std.List
  
  include struct
    type nonrec 'a0 peano_fuly =
      | O
      | S of 'a0
    [@@deriving gt ~options:{ show; fmt; gmap }]
  
    include struct
      let _ = fun (_ : 'a0 peano_fuly) -> ()
  
      class virtual ['ia0, 'a0, 'sa0, 'inh, 'extra, 'syn] peano_fuly_t =
        object
          method virtual c_O : 'inh -> 'extra -> 'syn
          method virtual c_S : 'inh -> 'extra -> 'a0 -> 'syn
        end
  
      let gcata_peano_fuly
        (tr : (_, 'typ0__005_, _, _, 'typ0__005_ peano_fuly, _) #peano_fuly_t)
        inh
        subj
        =
        match subj with
        | O -> tr#c_O inh subj
        | S _x__004_ -> tr#c_S inh subj _x__004_
      ;;
  
      let _ = gcata_peano_fuly
  
      class ['a0, 'a0_2, 'extra_peano_fuly, 'syn_peano_fuly] gmap_peano_fuly_t
        fa0
        _fself_peano_fuly =
        object
          inherit [unit, 'a0, 'a0_2, unit, 'extra_peano_fuly, 'a0_2 peano_fuly] peano_fuly_t
          constraint 'extra_peano_fuly = 'a0 peano_fuly
          constraint 'syn_peano_fuly = 'a0_2 peano_fuly
          method c_O () _ = O
          method c_S () _ _x__006_ = S (fa0 () _x__006_)
        end
  
      let gmap_peano_fuly fa0 inh0 subj =
        GT.transform_gc gcata_peano_fuly (new gmap_peano_fuly_t fa0) inh0 subj
      ;;
  
      let _ = gmap_peano_fuly
  
      class ['a0, 'extra_peano_fuly] fmt_peano_fuly_t fa0 _fself_peano_fuly =
        object
          inherit
            [Format.formatter, 'a0, unit, Format.formatter, 'extra_peano_fuly, unit] peano_fuly_t
  
          constraint 'extra_peano_fuly = 'a0 peano_fuly
  
          method c_O inh___007_ _ =
            Format.fprintf
              inh___007_
              (CamlinternalFormatBasics.Format
                 ( CamlinternalFormatBasics.Char_literal
                     ('O', CamlinternalFormatBasics.End_of_format)
                 , "O" ))
  
          method c_S inh___008_ _ _x__009_ =
            Format.fprintf
              inh___008_
              (CamlinternalFormatBasics.Format
                 ( CamlinternalFormatBasics.String_literal
                     ( "S "
                     , CamlinternalFormatBasics.Formatting_gen
                         ( CamlinternalFormatBasics.Open_box
                             (CamlinternalFormatBasics.Format
                                (CamlinternalFormatBasics.End_of_format, ""))
                         , CamlinternalFormatBasics.Char_literal
                             ( '('
                             , CamlinternalFormatBasics.Formatting_lit
                                 ( CamlinternalFormatBasics.Break ("@,", 0, 0)
                                 , CamlinternalFormatBasics.Alpha
                                     (CamlinternalFormatBasics.Formatting_lit
                                        ( CamlinternalFormatBasics.Break ("@,", 0, 0)
                                        , CamlinternalFormatBasics.Char_literal
                                            ( ')'
                                            , CamlinternalFormatBasics.Formatting_lit
                                                ( CamlinternalFormatBasics.Close_box
                                                , CamlinternalFormatBasics.End_of_format )
                                            ) )) ) ) ) )
                 , "S @[(@,%a@,)@]" ))
              fa0
              _x__009_
        end
  
      let fmt_peano_fuly fa0 inh0 subj =
        GT.transform_gc gcata_peano_fuly (new fmt_peano_fuly_t fa0) inh0 subj
      ;;
  
      let _ = fmt_peano_fuly
  
      class ['a0, 'extra_peano_fuly] show_peano_fuly_t fa0 _fself_peano_fuly =
        object
          inherit [unit, 'a0, string, unit, 'extra_peano_fuly, string] peano_fuly_t
          constraint 'extra_peano_fuly = 'a0 peano_fuly
  
          method c_O () _ =
            match () with
            | () -> "O"
  
          method c_S () _ _x__010_ =
            match () with
            | () ->
              Printf.sprintf
                (CamlinternalFormatBasics.Format
                   ( CamlinternalFormatBasics.String_literal
                       ( "S ("
                       , CamlinternalFormatBasics.String
                           ( CamlinternalFormatBasics.No_padding
                           , CamlinternalFormatBasics.Char_literal
                               (')', CamlinternalFormatBasics.End_of_format) ) )
                   , "S (%s)" ))
                (fa0 () _x__010_)
        end
  
      let show_peano_fuly fa0 inh0 subj =
        GT.transform_gc gcata_peano_fuly (new show_peano_fuly_t fa0) inh0 subj
      ;;
  
      let _ = show_peano_fuly
  
      let peano_fuly =
        { GT.fix = (fun eta -> GT.transform_gc gcata_peano_fuly eta)
        ; GT.plugins =
            object
              method gmap fa0 subj = gmap_peano_fuly (GT.lift fa0) () subj
              method fmt = fmt_peano_fuly
              method show fa0 subj = show_peano_fuly (GT.lift fa0) () subj
            end
        ; GT.gcata = gcata_peano_fuly
        }
      ;;
  
      let _ = peano_fuly
      let gmap_peano_fuly fa0 subj = gmap_peano_fuly (GT.lift fa0) () subj
      let _ = gmap_peano_fuly
      let show_peano_fuly fa0 subj = show_peano_fuly (GT.lift fa0) () subj
      let _ = show_peano_fuly
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type peano = peano peano_fuly [@@deriving gt ~options:{ show; fmt; gmap }]
  
    include struct
      let _ = fun (_ : peano) -> ()
  
      class virtual ['inh, 'extra, 'syn] peano_t =
        object
          inherit [peano, peano, peano, 'inh, 'extra, 'syn] peano_fuly_t
        end
  
      let gcata_peano = gcata_peano_fuly
      let _ = gcata_peano
  
      class ['extra_peano, 'syn_peano] gmap_peano_t _fself_peano =
        object
          inherit [unit, 'extra_peano, peano] peano_t
          constraint 'extra_peano = peano
          constraint 'syn_peano = peano
  
          inherit
            [peano, peano, 'extra_peano, 'syn_peano] gmap_peano_fuly_t
              _fself_peano
              _fself_peano
        end
  
      let rec gmap_peano () subj = GT.gmap peano_fuly (gmap_peano ()) subj
      let _ = gmap_peano
  
      class ['extra_peano] fmt_peano_t _fself_peano =
        object
          inherit [Format.formatter, 'extra_peano, unit] peano_t
          constraint 'extra_peano = peano
          inherit [peano, 'extra_peano] fmt_peano_fuly_t _fself_peano _fself_peano
        end
  
      let rec fmt_peano inh___011_ subj___012_ =
        GT.fmt peano_fuly fmt_peano inh___011_ subj___012_
      ;;
  
      let _ = fmt_peano
  
      class ['extra_peano] show_peano_t _fself_peano =
        object
          inherit [unit, 'extra_peano, string] peano_t
          constraint 'extra_peano = peano
          inherit [peano, 'extra_peano] show_peano_fuly_t _fself_peano _fself_peano
        end
  
      let rec show_peano () subj = GT.show peano_fuly (show_peano ()) subj
      let _ = show_peano
  
      let peano =
        { GT.fix = (fun eta -> GT.transform_gc gcata_peano eta)
        ; GT.plugins =
            object
              method gmap subj = gmap_peano () subj
              method fmt = fmt_peano
              method show subj = show_peano () subj
            end
        ; GT.gcata = gcata_peano
        }
      ;;
  
      let _ = peano
      let gmap_peano subj = gmap_peano () subj
      let _ = gmap_peano
      let show_peano subj = show_peano () subj
      let _ = show_peano
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type peano_logic = peano_logic peano_fuly OCanren.logic
    [@@deriving gt ~options:{ show; fmt; gmap }]
  
    include struct
      let _ = fun (_ : peano_logic) -> ()
  
      class virtual ['inh, 'extra, 'syn] peano_logic_t =
        object
          inherit
            [ peano_logic peano_fuly
            , peano_logic peano_fuly
            , peano_logic peano_fuly
            , 'inh
            , 'extra
            , 'syn ]
            OCanren.logic_t
        end
  
      let gcata_peano_logic = OCanren.gcata_logic
      let _ = gcata_peano_logic
  
      class ['extra_peano_logic, 'syn_peano_logic] gmap_peano_logic_t _fself_peano_logic =
        object
          inherit [unit, 'extra_peano_logic, peano_logic] peano_logic_t
          constraint 'extra_peano_logic = peano_logic
          constraint 'syn_peano_logic = peano_logic
  
          inherit
            [ peano_logic peano_fuly
            , peano_logic peano_fuly
            , 'extra_peano_logic
            , 'syn_peano_logic ]
            OCanren.gmap_logic_t
              (fun () subj -> GT.gmap peano_fuly (_fself_peano_logic ()) subj)
              _fself_peano_logic
        end
  
      let rec gmap_peano_logic () subj =
        GT.gmap
          OCanren.logic
          ((fun () subj -> GT.gmap peano_fuly (gmap_peano_logic ()) subj) ())
          subj
      ;;
  
      let _ = gmap_peano_logic
  
      class ['extra_peano_logic] fmt_peano_logic_t _fself_peano_logic =
        object
          inherit [Format.formatter, 'extra_peano_logic, unit] peano_logic_t
          constraint 'extra_peano_logic = peano_logic
  
          inherit
            [peano_logic peano_fuly, 'extra_peano_logic] OCanren.fmt_logic_t
              (fun inh___017_ subj___018_ ->
                GT.fmt peano_fuly _fself_peano_logic inh___017_ subj___018_)
              _fself_peano_logic
        end
  
      let rec fmt_peano_logic inh___013_ subj___014_ =
        GT.fmt
          OCanren.logic
          (fun inh___015_ subj___016_ ->
            GT.fmt peano_fuly fmt_peano_logic inh___015_ subj___016_)
          inh___013_
          subj___014_
      ;;
  
      let _ = fmt_peano_logic
  
      class ['extra_peano_logic] show_peano_logic_t _fself_peano_logic =
        object
          inherit [unit, 'extra_peano_logic, string] peano_logic_t
          constraint 'extra_peano_logic = peano_logic
  
          inherit
            [peano_logic peano_fuly, 'extra_peano_logic] OCanren.show_logic_t
              (fun () subj -> GT.show peano_fuly (_fself_peano_logic ()) subj)
              _fself_peano_logic
        end
  
      let rec show_peano_logic () subj =
        GT.show
          OCanren.logic
          ((fun () subj -> GT.show peano_fuly (show_peano_logic ()) subj) ())
          subj
      ;;
  
      let _ = show_peano_logic
  
      let peano_logic =
        { GT.fix = (fun eta -> GT.transform_gc gcata_peano_logic eta)
        ; GT.plugins =
            object
              method gmap subj = gmap_peano_logic () subj
              method fmt = fmt_peano_logic
              method show subj = show_peano_logic () subj
            end
        ; GT.gcata = gcata_peano_logic
        }
      ;;
  
      let _ = peano_logic
      let gmap_peano_logic subj = gmap_peano_logic () subj
      let _ = gmap_peano_logic
      let show_peano_logic subj = show_peano_logic () subj
      let _ = show_peano_logic
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type peano_injected = peano_injected peano_fuly OCanren.ilogic
  
    let peano_fmapt f__002_ subj__003_ =
      let open OCanren.Env.Monad in
      OCanren.Env.Monad.return (GT.gmap peano_fuly) <*> f__002_ <*> subj__003_
    ;;
  
    let (peano_prj_exn : (peano_injected, peano) OCanren.Reifier.t) =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun self -> OCanren.prj_exn <..> chain (peano_fmapt self))
    ;;
  
    let (peano_reify : (peano_injected, peano_logic) OCanren.Reifier.t) =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun self ->
        OCanren.reify
        <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(peano_fmapt self))))
    ;;
  end
  
  include struct
    type nonrec person_fuly =
      | A
      | B
      | C
      | D
    [@@deriving gt ~options:{ show; fmt; gmap }]
  
    include struct
      let _ = fun (_ : person_fuly) -> ()
  
      class virtual ['inh, 'extra, 'syn] person_fuly_t =
        object
          method virtual c_A : 'inh -> 'extra -> 'syn
          method virtual c_B : 'inh -> 'extra -> 'syn
          method virtual c_C : 'inh -> 'extra -> 'syn
          method virtual c_D : 'inh -> 'extra -> 'syn
        end
  
      let gcata_person_fuly (tr : (_, person_fuly, _) #person_fuly_t) inh subj =
        match subj with
        | A -> tr#c_A inh subj
        | B -> tr#c_B inh subj
        | C -> tr#c_C inh subj
        | D -> tr#c_D inh subj
      ;;
  
      let _ = gcata_person_fuly
  
      class ['extra_person_fuly, 'syn_person_fuly] gmap_person_fuly_t _fself_person_fuly =
        object
          inherit [unit, 'extra_person_fuly, person_fuly] person_fuly_t
          constraint 'extra_person_fuly = person_fuly
          constraint 'syn_person_fuly = person_fuly
          method c_A () _ = A
          method c_B () _ = B
          method c_C () _ = C
          method c_D () _ = D
        end
  
      let gmap_person_fuly inh0 subj =
        GT.transform_gc gcata_person_fuly (new gmap_person_fuly_t) inh0 subj
      ;;
  
      let _ = gmap_person_fuly
  
      class ['extra_person_fuly] fmt_person_fuly_t _fself_person_fuly =
        object
          inherit [Format.formatter, 'extra_person_fuly, unit] person_fuly_t
          constraint 'extra_person_fuly = person_fuly
  
          method c_A inh___020_ _ =
            Format.fprintf
              inh___020_
              (CamlinternalFormatBasics.Format
                 ( CamlinternalFormatBasics.Char_literal
                     ('A', CamlinternalFormatBasics.End_of_format)
                 , "A" ))
  
          method c_B inh___021_ _ =
            Format.fprintf
              inh___021_
              (CamlinternalFormatBasics.Format
                 ( CamlinternalFormatBasics.Char_literal
                     ('B', CamlinternalFormatBasics.End_of_format)
                 , "B" ))
  
          method c_C inh___022_ _ =
            Format.fprintf
              inh___022_
              (CamlinternalFormatBasics.Format
                 ( CamlinternalFormatBasics.Char_literal
                     ('C', CamlinternalFormatBasics.End_of_format)
                 , "C" ))
  
          method c_D inh___023_ _ =
            Format.fprintf
              inh___023_
              (CamlinternalFormatBasics.Format
                 ( CamlinternalFormatBasics.Char_literal
                     ('D', CamlinternalFormatBasics.End_of_format)
                 , "D" ))
        end
  
      let fmt_person_fuly inh0 subj =
        GT.transform_gc gcata_person_fuly (new fmt_person_fuly_t) inh0 subj
      ;;
  
      let _ = fmt_person_fuly
  
      class ['extra_person_fuly] show_person_fuly_t _fself_person_fuly =
        object
          inherit [unit, 'extra_person_fuly, string] person_fuly_t
          constraint 'extra_person_fuly = person_fuly
  
          method c_A () _ =
            match () with
            | () -> "A"
  
          method c_B () _ =
            match () with
            | () -> "B"
  
          method c_C () _ =
            match () with
            | () -> "C"
  
          method c_D () _ =
            match () with
            | () -> "D"
        end
  
      let show_person_fuly inh0 subj =
        GT.transform_gc gcata_person_fuly (new show_person_fuly_t) inh0 subj
      ;;
  
      let _ = show_person_fuly
  
      let person_fuly =
        { GT.fix = (fun eta -> GT.transform_gc gcata_person_fuly eta)
        ; GT.plugins =
            object
              method gmap subj = gmap_person_fuly () subj
              method fmt = fmt_person_fuly
              method show subj = show_person_fuly () subj
            end
        ; GT.gcata = gcata_person_fuly
        }
      ;;
  
      let _ = person_fuly
      let gmap_person_fuly subj = gmap_person_fuly () subj
      let _ = gmap_person_fuly
      let show_person_fuly subj = show_person_fuly () subj
      let _ = show_person_fuly
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type person = person_fuly [@@deriving gt ~options:{ show; fmt; gmap }]
  
    include struct
      let _ = fun (_ : person) -> ()
  
      class virtual ['inh, 'extra, 'syn] person_t =
        object
          inherit ['inh, 'extra, 'syn] person_fuly_t
        end
  
      let gcata_person = gcata_person_fuly
      let _ = gcata_person
  
      class ['extra_person, 'syn_person] gmap_person_t _fself_person =
        object
          inherit [unit, 'extra_person, person] person_t
          constraint 'extra_person = person
          constraint 'syn_person = person
          inherit ['extra_person, 'syn_person] gmap_person_fuly_t _fself_person
        end
  
      let gmap_person () subj = GT.gmap person_fuly subj
      let _ = gmap_person
  
      class ['extra_person] fmt_person_t _fself_person =
        object
          inherit [Format.formatter, 'extra_person, unit] person_t
          constraint 'extra_person = person
          inherit ['extra_person] fmt_person_fuly_t _fself_person
        end
  
      let fmt_person inh___024_ subj___025_ = GT.fmt person_fuly inh___024_ subj___025_
      let _ = fmt_person
  
      class ['extra_person] show_person_t _fself_person =
        object
          inherit [unit, 'extra_person, string] person_t
          constraint 'extra_person = person
          inherit ['extra_person] show_person_fuly_t _fself_person
        end
  
      let show_person () subj = GT.show person_fuly subj
      let _ = show_person
  
      let person =
        { GT.fix = (fun eta -> GT.transform_gc gcata_person eta)
        ; GT.plugins =
            object
              method gmap subj = gmap_person () subj
              method fmt = fmt_person
              method show subj = show_person () subj
            end
        ; GT.gcata = gcata_person
        }
      ;;
  
      let _ = person
      let gmap_person subj = gmap_person () subj
      let _ = gmap_person
      let show_person subj = show_person () subj
      let _ = show_person
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type person_logic = person_fuly OCanren.logic
    [@@deriving gt ~options:{ show; fmt; gmap }]
  
    include struct
      let _ = fun (_ : person_logic) -> ()
  
      class virtual ['inh, 'extra, 'syn] person_logic_t =
        object
          inherit
            [person_fuly, person_fuly, person_fuly, 'inh, 'extra, 'syn] OCanren.logic_t
        end
  
      let gcata_person_logic = OCanren.gcata_logic
      let _ = gcata_person_logic
  
      class ['extra_person_logic, 'syn_person_logic] gmap_person_logic_t _fself_person_logic
        =
        object
          inherit [unit, 'extra_person_logic, person_logic] person_logic_t
          constraint 'extra_person_logic = person_logic
          constraint 'syn_person_logic = person_logic
  
          inherit
            [person_fuly, person_fuly, 'extra_person_logic, 'syn_person_logic] OCanren
                                                                               .gmap_logic_t
              (fun () subj -> GT.gmap person_fuly subj)
              _fself_person_logic
        end
  
      let gmap_person_logic () subj =
        GT.gmap OCanren.logic ((fun () subj -> GT.gmap person_fuly subj) ()) subj
      ;;
  
      let _ = gmap_person_logic
  
      class ['extra_person_logic] fmt_person_logic_t _fself_person_logic =
        object
          inherit [Format.formatter, 'extra_person_logic, unit] person_logic_t
          constraint 'extra_person_logic = person_logic
  
          inherit
            [person_fuly, 'extra_person_logic] OCanren.fmt_logic_t
              (fun inh___030_ subj___031_ -> GT.fmt person_fuly inh___030_ subj___031_)
              _fself_person_logic
        end
  
      let fmt_person_logic inh___026_ subj___027_ =
        GT.fmt
          OCanren.logic
          (fun inh___028_ subj___029_ -> GT.fmt person_fuly inh___028_ subj___029_)
          inh___026_
          subj___027_
      ;;
  
      let _ = fmt_person_logic
  
      class ['extra_person_logic] show_person_logic_t _fself_person_logic =
        object
          inherit [unit, 'extra_person_logic, string] person_logic_t
          constraint 'extra_person_logic = person_logic
  
          inherit
            [person_fuly, 'extra_person_logic] OCanren.show_logic_t
              (fun () subj -> GT.show person_fuly subj)
              _fself_person_logic
        end
  
      let show_person_logic () subj =
        GT.show OCanren.logic ((fun () subj -> GT.show person_fuly subj) ()) subj
      ;;
  
      let _ = show_person_logic
  
      let person_logic =
        { GT.fix = (fun eta -> GT.transform_gc gcata_person_logic eta)
        ; GT.plugins =
            object
              method gmap subj = gmap_person_logic () subj
              method fmt = fmt_person_logic
              method show subj = show_person_logic () subj
            end
        ; GT.gcata = gcata_person_logic
        }
      ;;
  
      let _ = person_logic
      let gmap_person_logic subj = gmap_person_logic () subj
      let _ = gmap_person_logic
      let show_person_logic subj = show_person_logic () subj
      let _ = show_person_logic
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type person_injected = person_fuly OCanren.ilogic
  
    let person_fmapt subj__019_ =
      let open OCanren.Env.Monad in
      OCanren.Env.Monad.return (GT.gmap person_fuly) <*> subj__019_
    ;;
  
    let (person_prj_exn : (person_injected, person) OCanren.Reifier.t) =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun self -> OCanren.prj_exn <..> chain person_fmapt)
    ;;
  
    let (person_reify : (person_injected, person_logic) OCanren.Reifier.t) =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun self ->
        OCanren.reify
        <..> chain (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:person_fmapt)))
    ;;
  end
  
  include struct
    type nonrec 'a0 step_fuly =
      | One of 'a0
      | Two of 'a0 * 'a0
    [@@deriving gt ~options:{ show; fmt; gmap }]
  
    include struct
      let _ = fun (_ : 'a0 step_fuly) -> ()
  
      class virtual ['ia0, 'a0, 'sa0, 'inh, 'extra, 'syn] step_fuly_t =
        object
          method virtual c_One : 'inh -> 'extra -> 'a0 -> 'syn
          method virtual c_Two : 'inh -> 'extra -> 'a0 -> 'a0 -> 'syn
        end
  
      let gcata_step_fuly
        (tr : (_, 'typ0__038_, _, _, 'typ0__038_ step_fuly, _) #step_fuly_t)
        inh
        subj
        =
        match subj with
        | One _x__035_ -> tr#c_One inh subj _x__035_
        | Two (_x__036_, _x__037_) -> tr#c_Two inh subj _x__036_ _x__037_
      ;;
  
      let _ = gcata_step_fuly
  
      class ['a0, 'a0_2, 'extra_step_fuly, 'syn_step_fuly] gmap_step_fuly_t
        fa0
        _fself_step_fuly =
        object
          inherit [unit, 'a0, 'a0_2, unit, 'extra_step_fuly, 'a0_2 step_fuly] step_fuly_t
          constraint 'extra_step_fuly = 'a0 step_fuly
          constraint 'syn_step_fuly = 'a0_2 step_fuly
          method c_One () _ _x__039_ = One (fa0 () _x__039_)
          method c_Two () _ _x__040_ _x__041_ = Two (fa0 () _x__040_, fa0 () _x__041_)
        end
  
      let gmap_step_fuly fa0 inh0 subj =
        GT.transform_gc gcata_step_fuly (new gmap_step_fuly_t fa0) inh0 subj
      ;;
  
      let _ = gmap_step_fuly
  
      class ['a0, 'extra_step_fuly] fmt_step_fuly_t fa0 _fself_step_fuly =
        object
          inherit
            [Format.formatter, 'a0, unit, Format.formatter, 'extra_step_fuly, unit] step_fuly_t
  
          constraint 'extra_step_fuly = 'a0 step_fuly
  
          method c_One inh___042_ _ _x__043_ =
            Format.fprintf
              inh___042_
              (CamlinternalFormatBasics.Format
                 ( CamlinternalFormatBasics.String_literal
                     ( "One "
                     , CamlinternalFormatBasics.Formatting_gen
                         ( CamlinternalFormatBasics.Open_box
                             (CamlinternalFormatBasics.Format
                                (CamlinternalFormatBasics.End_of_format, ""))
                         , CamlinternalFormatBasics.Char_literal
                             ( '('
                             , CamlinternalFormatBasics.Formatting_lit
                                 ( CamlinternalFormatBasics.Break ("@,", 0, 0)
                                 , CamlinternalFormatBasics.Alpha
                                     (CamlinternalFormatBasics.Formatting_lit
                                        ( CamlinternalFormatBasics.Break ("@,", 0, 0)
                                        , CamlinternalFormatBasics.Char_literal
                                            ( ')'
                                            , CamlinternalFormatBasics.Formatting_lit
                                                ( CamlinternalFormatBasics.Close_box
                                                , CamlinternalFormatBasics.End_of_format )
                                            ) )) ) ) ) )
                 , "One @[(@,%a@,)@]" ))
              fa0
              _x__043_
  
          method c_Two inh___044_ _ _x__045_ _x__046_ =
            Format.fprintf
              inh___044_
              (CamlinternalFormatBasics.Format
                 ( CamlinternalFormatBasics.String_literal
                     ( "Two "
                     , CamlinternalFormatBasics.Formatting_gen
                         ( CamlinternalFormatBasics.Open_box
                             (CamlinternalFormatBasics.Format
                                (CamlinternalFormatBasics.End_of_format, ""))
                         , CamlinternalFormatBasics.Char_literal
                             ( '('
                             , CamlinternalFormatBasics.Formatting_lit
                                 ( CamlinternalFormatBasics.Break ("@,", 0, 0)
                                 , CamlinternalFormatBasics.Alpha
                                     (CamlinternalFormatBasics.Char_literal
                                        ( ','
                                        , CamlinternalFormatBasics.Formatting_lit
                                            ( CamlinternalFormatBasics.Break ("@,", 0, 0)
                                            , CamlinternalFormatBasics.Formatting_lit
                                                ( CamlinternalFormatBasics.Break ("@ ", 1, 0)
                                                , CamlinternalFormatBasics.Alpha
                                                    (CamlinternalFormatBasics.Formatting_lit
                                                       ( CamlinternalFormatBasics.Break
                                                           ("@,", 0, 0)
                                                       , CamlinternalFormatBasics
                                                         .Char_literal
                                                           ( ')'
                                                           , CamlinternalFormatBasics
                                                             .Formatting_lit
                                                               ( CamlinternalFormatBasics
                                                                 .Close_box
                                                               , CamlinternalFormatBasics
                                                                 .End_of_format ) ) )) ) )
                                        )) ) ) ) )
                 , "Two @[(@,%a,@,@ %a@,)@]" ))
              fa0
              _x__045_
              fa0
              _x__046_
        end
  
      let fmt_step_fuly fa0 inh0 subj =
        GT.transform_gc gcata_step_fuly (new fmt_step_fuly_t fa0) inh0 subj
      ;;
  
      let _ = fmt_step_fuly
  
      class ['a0, 'extra_step_fuly] show_step_fuly_t fa0 _fself_step_fuly =
        object
          inherit [unit, 'a0, string, unit, 'extra_step_fuly, string] step_fuly_t
          constraint 'extra_step_fuly = 'a0 step_fuly
  
          method c_One () _ _x__047_ =
            match () with
            | () ->
              Printf.sprintf
                (CamlinternalFormatBasics.Format
                   ( CamlinternalFormatBasics.String_literal
                       ( "One ("
                       , CamlinternalFormatBasics.String
                           ( CamlinternalFormatBasics.No_padding
                           , CamlinternalFormatBasics.Char_literal
                               (')', CamlinternalFormatBasics.End_of_format) ) )
                   , "One (%s)" ))
                (fa0 () _x__047_)
  
          method c_Two () _ _x__048_ _x__049_ =
            match () with
            | () ->
              Printf.sprintf
                (CamlinternalFormatBasics.Format
                   ( CamlinternalFormatBasics.String_literal
                       ( "Two ("
                       , CamlinternalFormatBasics.String
                           ( CamlinternalFormatBasics.No_padding
                           , CamlinternalFormatBasics.String_literal
                               ( ", "
                               , CamlinternalFormatBasics.String
                                   ( CamlinternalFormatBasics.No_padding
                                   , CamlinternalFormatBasics.Char_literal
                                       (')', CamlinternalFormatBasics.End_of_format) ) ) )
                       )
                   , "Two (%s, %s)" ))
                (fa0 () _x__048_)
                (fa0 () _x__049_)
        end
  
      let show_step_fuly fa0 inh0 subj =
        GT.transform_gc gcata_step_fuly (new show_step_fuly_t fa0) inh0 subj
      ;;
  
      let _ = show_step_fuly
  
      let step_fuly =
        { GT.fix = (fun eta -> GT.transform_gc gcata_step_fuly eta)
        ; GT.plugins =
            object
              method gmap fa0 subj = gmap_step_fuly (GT.lift fa0) () subj
              method fmt = fmt_step_fuly
              method show fa0 subj = show_step_fuly (GT.lift fa0) () subj
            end
        ; GT.gcata = gcata_step_fuly
        }
      ;;
  
      let _ = step_fuly
      let gmap_step_fuly fa0 subj = gmap_step_fuly (GT.lift fa0) () subj
      let _ = gmap_step_fuly
      let show_step_fuly fa0 subj = show_step_fuly (GT.lift fa0) () subj
      let _ = show_step_fuly
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type step = person step_fuly [@@deriving gt ~options:{ show; fmt; gmap }]
  
    include struct
      let _ = fun (_ : step) -> ()
  
      class virtual ['inh, 'extra, 'syn] step_t =
        object
          inherit [person, person, person, 'inh, 'extra, 'syn] step_fuly_t
        end
  
      let gcata_step = gcata_step_fuly
      let _ = gcata_step
  
      class ['extra_step, 'syn_step] gmap_step_t _fself_step =
        object
          inherit [unit, 'extra_step, step] step_t
          constraint 'extra_step = step
          constraint 'syn_step = step
  
          inherit
            [person, person, 'extra_step, 'syn_step] gmap_step_fuly_t
              (fun () subj -> GT.gmap person subj)
              _fself_step
        end
  
      let gmap_step () subj =
        GT.gmap step_fuly ((fun () subj -> GT.gmap person subj) ()) subj
      ;;
  
      let _ = gmap_step
  
      class ['extra_step] fmt_step_t _fself_step =
        object
          inherit [Format.formatter, 'extra_step, unit] step_t
          constraint 'extra_step = step
  
          inherit
            [person, 'extra_step] fmt_step_fuly_t
              (fun inh___054_ subj___055_ -> GT.fmt person inh___054_ subj___055_)
              _fself_step
        end
  
      let fmt_step inh___050_ subj___051_ =
        GT.fmt
          step_fuly
          (fun inh___052_ subj___053_ -> GT.fmt person inh___052_ subj___053_)
          inh___050_
          subj___051_
      ;;
  
      let _ = fmt_step
  
      class ['extra_step] show_step_t _fself_step =
        object
          inherit [unit, 'extra_step, string] step_t
          constraint 'extra_step = step
  
          inherit
            [person, 'extra_step] show_step_fuly_t
              (fun () subj -> GT.show person subj)
              _fself_step
        end
  
      let show_step () subj =
        GT.show step_fuly ((fun () subj -> GT.show person subj) ()) subj
      ;;
  
      let _ = show_step
  
      let step =
        { GT.fix = (fun eta -> GT.transform_gc gcata_step eta)
        ; GT.plugins =
            object
              method gmap subj = gmap_step () subj
              method fmt = fmt_step
              method show subj = show_step () subj
            end
        ; GT.gcata = gcata_step
        }
      ;;
  
      let _ = step
      let gmap_step subj = gmap_step () subj
      let _ = gmap_step
      let show_step subj = show_step () subj
      let _ = show_step
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type step_logic = person_logic step_fuly OCanren.logic
    [@@deriving gt ~options:{ show; fmt; gmap }]
  
    include struct
      let _ = fun (_ : step_logic) -> ()
  
      class virtual ['inh, 'extra, 'syn] step_logic_t =
        object
          inherit
            [ person_logic step_fuly
            , person_logic step_fuly
            , person_logic step_fuly
            , 'inh
            , 'extra
            , 'syn ]
            OCanren.logic_t
        end
  
      let gcata_step_logic = OCanren.gcata_logic
      let _ = gcata_step_logic
  
      class ['extra_step_logic, 'syn_step_logic] gmap_step_logic_t _fself_step_logic =
        object
          inherit [unit, 'extra_step_logic, step_logic] step_logic_t
          constraint 'extra_step_logic = step_logic
          constraint 'syn_step_logic = step_logic
  
          inherit
            [ person_logic step_fuly
            , person_logic step_fuly
            , 'extra_step_logic
            , 'syn_step_logic ]
            OCanren.gmap_logic_t
              (fun () subj ->
                GT.gmap step_fuly ((fun () subj -> GT.gmap person_logic subj) ()) subj)
              _fself_step_logic
        end
  
      let gmap_step_logic () subj =
        GT.gmap
          OCanren.logic
          ((fun () subj ->
             GT.gmap step_fuly ((fun () subj -> GT.gmap person_logic subj) ()) subj)
             ())
          subj
      ;;
  
      let _ = gmap_step_logic
  
      class ['extra_step_logic] fmt_step_logic_t _fself_step_logic =
        object
          inherit [Format.formatter, 'extra_step_logic, unit] step_logic_t
          constraint 'extra_step_logic = step_logic
  
          inherit
            [person_logic step_fuly, 'extra_step_logic] OCanren.fmt_logic_t
              (fun inh___062_ subj___063_ ->
                GT.fmt
                  step_fuly
                  (fun inh___064_ subj___065_ -> GT.fmt person_logic inh___064_ subj___065_)
                  inh___062_
                  subj___063_)
              _fself_step_logic
        end
  
      let fmt_step_logic inh___056_ subj___057_ =
        GT.fmt
          OCanren.logic
          (fun inh___058_ subj___059_ ->
            GT.fmt
              step_fuly
              (fun inh___060_ subj___061_ -> GT.fmt person_logic inh___060_ subj___061_)
              inh___058_
              subj___059_)
          inh___056_
          subj___057_
      ;;
  
      let _ = fmt_step_logic
  
      class ['extra_step_logic] show_step_logic_t _fself_step_logic =
        object
          inherit [unit, 'extra_step_logic, string] step_logic_t
          constraint 'extra_step_logic = step_logic
  
          inherit
            [person_logic step_fuly, 'extra_step_logic] OCanren.show_logic_t
              (fun () subj ->
                GT.show step_fuly ((fun () subj -> GT.show person_logic subj) ()) subj)
              _fself_step_logic
        end
  
      let show_step_logic () subj =
        GT.show
          OCanren.logic
          ((fun () subj ->
             GT.show step_fuly ((fun () subj -> GT.show person_logic subj) ()) subj)
             ())
          subj
      ;;
  
      let _ = show_step_logic
  
      let step_logic =
        { GT.fix = (fun eta -> GT.transform_gc gcata_step_logic eta)
        ; GT.plugins =
            object
              method gmap subj = gmap_step_logic () subj
              method fmt = fmt_step_logic
              method show subj = show_step_logic () subj
            end
        ; GT.gcata = gcata_step_logic
        }
      ;;
  
      let _ = step_logic
      let gmap_step_logic subj = gmap_step_logic () subj
      let _ = gmap_step_logic
      let show_step_logic subj = show_step_logic () subj
      let _ = show_step_logic
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type step_injected = person_injected step_fuly OCanren.ilogic
  
    let step_fmapt f__033_ subj__034_ =
      let open OCanren.Env.Monad in
      OCanren.Env.Monad.return (GT.gmap step_fuly) <*> f__033_ <*> subj__034_
    ;;
  
    let (step_prj_exn : (step_injected, step) OCanren.Reifier.t) =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun self ->
        OCanren.prj_exn <..> chain (step_fmapt person_prj_exn))
    ;;
  
    let (step_reify : (step_injected, step_logic) OCanren.Reifier.t) =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun self ->
        OCanren.reify
        <..> chain
               (OCanren.Reifier.zed (OCanren.Reifier.rework ~fv:(step_fmapt person_reify))))
    ;;
  end
  
  include struct
    type nonrec 'a0 state_fuly = St of 'a0 * 'a0 * 'a0 * 'a0 * 'a0
    [@@deriving gt ~options:{ show; fmt; gmap }]
  
    include struct
      let _ = fun (_ : 'a0 state_fuly) -> ()
  
      class virtual ['ia0, 'a0, 'sa0, 'inh, 'extra, 'syn] state_fuly_t =
        object
          method virtual c_St : 'inh -> 'extra -> 'a0 -> 'a0 -> 'a0 -> 'a0 -> 'a0 -> 'syn
        end
  
      let gcata_state_fuly
        (tr : (_, 'typ0__074_, _, _, 'typ0__074_ state_fuly, _) #state_fuly_t)
        inh
        subj
        =
        match subj with
        | St (_x__069_, _x__070_, _x__071_, _x__072_, _x__073_) ->
          tr#c_St inh subj _x__069_ _x__070_ _x__071_ _x__072_ _x__073_
      ;;
  
      let _ = gcata_state_fuly
  
      class ['a0, 'a0_2, 'extra_state_fuly, 'syn_state_fuly] gmap_state_fuly_t
        fa0
        _fself_state_fuly =
        object
          inherit [unit, 'a0, 'a0_2, unit, 'extra_state_fuly, 'a0_2 state_fuly] state_fuly_t
          constraint 'extra_state_fuly = 'a0 state_fuly
          constraint 'syn_state_fuly = 'a0_2 state_fuly
  
          method c_St () _ _x__075_ _x__076_ _x__077_ _x__078_ _x__079_ =
            St
              ( fa0 () _x__075_
              , fa0 () _x__076_
              , fa0 () _x__077_
              , fa0 () _x__078_
              , fa0 () _x__079_ )
        end
  
      let gmap_state_fuly fa0 inh0 subj =
        GT.transform_gc gcata_state_fuly (new gmap_state_fuly_t fa0) inh0 subj
      ;;
  
      let _ = gmap_state_fuly
  
      class ['a0, 'extra_state_fuly] fmt_state_fuly_t fa0 _fself_state_fuly =
        object
          inherit
            [Format.formatter, 'a0, unit, Format.formatter, 'extra_state_fuly, unit] state_fuly_t
  
          constraint 'extra_state_fuly = 'a0 state_fuly
  
          method c_St inh___080_ _ _x__081_ _x__082_ _x__083_ _x__084_ _x__085_ =
            Format.fprintf
              inh___080_
              (CamlinternalFormatBasics.Format
                 ( CamlinternalFormatBasics.String_literal
                     ( "St "
                     , CamlinternalFormatBasics.Formatting_gen
                         ( CamlinternalFormatBasics.Open_box
                             (CamlinternalFormatBasics.Format
                                (CamlinternalFormatBasics.End_of_format, ""))
                         , CamlinternalFormatBasics.Char_literal
                             ( '('
                             , CamlinternalFormatBasics.Formatting_lit
                                 ( CamlinternalFormatBasics.Break ("@,", 0, 0)
                                 , CamlinternalFormatBasics.Alpha
                                     (CamlinternalFormatBasics.Char_literal
                                        ( ','
                                        , CamlinternalFormatBasics.Formatting_lit
                                            ( CamlinternalFormatBasics.Break ("@,", 0, 0)
                                            , CamlinternalFormatBasics.Formatting_lit
                                                ( CamlinternalFormatBasics.Break ("@ ", 1, 0)
                                                , CamlinternalFormatBasics.Alpha
                                                    (CamlinternalFormatBasics.Char_literal
                                                       ( ','
                                                       , CamlinternalFormatBasics
                                                         .Formatting_lit
                                                           ( CamlinternalFormatBasics.Break
                                                               ("@,", 0, 0)
                                                           , CamlinternalFormatBasics
                                                             .Formatting_lit
                                                               ( CamlinternalFormatBasics
                                                                 .Break
                                                                   ("@ ", 1, 0)
                                                               , CamlinternalFormatBasics
                                                                 .Alpha
                                                                   (CamlinternalFormatBasics
                                                                    .Char_literal
                                                                      ( ','
                                                                      , CamlinternalFormatBasics
                                                                        .Formatting_lit
                                                                          ( CamlinternalFormatBasics
                                                                            .Break
                                                                              ("@,", 0, 0)
                                                                          , CamlinternalFormatBasics
                                                                            .Formatting_lit
                                                                              ( CamlinternalFormatBasics
                                                                                .Break
                                                                                  ( "@ "
                                                                                  , 1
                                                                                  , 0 )
                                                                              , CamlinternalFormatBasics
                                                                                .Alpha
                                                                                  (CamlinternalFormatBasics
                                                                                   .Char_literal
                                                                                     ( ','
                                                                                     , CamlinternalFormatBasics
                                                                                       .Formatting_lit
                                                                                         ( CamlinternalFormatBasics
                                                                                           .Break
                                                                                            ( 
                                                                                            "@,"
                                                                                            , 
                                                                                            0
                                                                                            , 
                                                                                            0
                                                                                            )
                                                                                         , CamlinternalFormatBasics
                                                                                           .Formatting_lit
                                                                                            ( 
                                                                                            CamlinternalFormatBasics
                                                                                            .Break
                                                                                            ( 
                                                                                            "@ "
                                                                                            , 
                                                                                            1
                                                                                            , 
                                                                                            0
                                                                                            )
                                                                                            , 
                                                                                            CamlinternalFormatBasics
                                                                                            .Alpha
                                                                                            (
                                                                                            CamlinternalFormatBasics
                                                                                            .Formatting_lit
                                                                                            ( 
                                                                                            CamlinternalFormatBasics
                                                                                            .Break
                                                                                            ( 
                                                                                            "@,"
                                                                                            , 
                                                                                            0
                                                                                            , 
                                                                                            0
                                                                                            )
                                                                                            , 
                                                                                            CamlinternalFormatBasics
                                                                                            .Char_literal
                                                                                            ( 
                                                                                            ')'
                                                                                            , 
                                                                                            CamlinternalFormatBasics
                                                                                            .Formatting_lit
                                                                                            ( 
                                                                                            CamlinternalFormatBasics
                                                                                            .Close_box
                                                                                            , 
                                                                                            CamlinternalFormatBasics
                                                                                            .End_of_format
                                                                                            )
                                                                                            )
                                                                                            ))
                                                                                            )
                                                                                         )
                                                                                     )) ) )
                                                                      )) ) ) )) ) ) )) ) )
                         ) )
                 , "St @[(@,%a,@,@ %a,@,@ %a,@,@ %a,@,@ %a@,)@]" ))
              fa0
              _x__081_
              fa0
              _x__082_
              fa0
              _x__083_
              fa0
              _x__084_
              fa0
              _x__085_
        end
  
      let fmt_state_fuly fa0 inh0 subj =
        GT.transform_gc gcata_state_fuly (new fmt_state_fuly_t fa0) inh0 subj
      ;;
  
      let _ = fmt_state_fuly
  
      class ['a0, 'extra_state_fuly] show_state_fuly_t fa0 _fself_state_fuly =
        object
          inherit [unit, 'a0, string, unit, 'extra_state_fuly, string] state_fuly_t
          constraint 'extra_state_fuly = 'a0 state_fuly
  
          method c_St () _ _x__086_ _x__087_ _x__088_ _x__089_ _x__090_ =
            match () with
            | () ->
              Printf.sprintf
                (CamlinternalFormatBasics.Format
                   ( CamlinternalFormatBasics.String_literal
                       ( "St ("
                       , CamlinternalFormatBasics.String
                           ( CamlinternalFormatBasics.No_padding
                           , CamlinternalFormatBasics.String_literal
                               ( ", "
                               , CamlinternalFormatBasics.String
                                   ( CamlinternalFormatBasics.No_padding
                                   , CamlinternalFormatBasics.String_literal
                                       ( ", "
                                       , CamlinternalFormatBasics.String
                                           ( CamlinternalFormatBasics.No_padding
                                           , CamlinternalFormatBasics.String_literal
                                               ( ", "
                                               , CamlinternalFormatBasics.String
                                                   ( CamlinternalFormatBasics.No_padding
                                                   , CamlinternalFormatBasics.String_literal
                                                       ( ", "
                                                       , CamlinternalFormatBasics.String
                                                           ( CamlinternalFormatBasics
                                                             .No_padding
                                                           , CamlinternalFormatBasics
                                                             .Char_literal
                                                               ( ')'
                                                               , CamlinternalFormatBasics
                                                                 .End_of_format ) ) ) ) ) )
                                       ) ) ) ) )
                   , "St (%s, %s, %s, %s, %s)" ))
                (fa0 () _x__086_)
                (fa0 () _x__087_)
                (fa0 () _x__088_)
                (fa0 () _x__089_)
                (fa0 () _x__090_)
        end
  
      let show_state_fuly fa0 inh0 subj =
        GT.transform_gc gcata_state_fuly (new show_state_fuly_t fa0) inh0 subj
      ;;
  
      let _ = show_state_fuly
  
      let state_fuly =
        { GT.fix = (fun eta -> GT.transform_gc gcata_state_fuly eta)
        ; GT.plugins =
            object
              method gmap fa0 subj = gmap_state_fuly (GT.lift fa0) () subj
              method fmt = fmt_state_fuly
              method show fa0 subj = show_state_fuly (GT.lift fa0) () subj
            end
        ; GT.gcata = gcata_state_fuly
        }
      ;;
  
      let _ = state_fuly
      let gmap_state_fuly fa0 subj = gmap_state_fuly (GT.lift fa0) () subj
      let _ = gmap_state_fuly
      let show_state_fuly fa0 subj = show_state_fuly (GT.lift fa0) () subj
      let _ = show_state_fuly
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type state = bool state_fuly [@@deriving gt ~options:{ show; fmt; gmap }]
  
    include struct
      let _ = fun (_ : state) -> ()
  
      class virtual ['inh, 'extra, 'syn] state_t =
        object
          inherit [bool, bool, bool, 'inh, 'extra, 'syn] state_fuly_t
        end
  
      let gcata_state = gcata_state_fuly
      let _ = gcata_state
  
      class ['extra_state, 'syn_state] gmap_state_t _fself_state =
        object
          inherit [unit, 'extra_state, state] state_t
          constraint 'extra_state = state
          constraint 'syn_state = state
  
          inherit
            [bool, bool, 'extra_state, 'syn_state] gmap_state_fuly_t
              (fun () subj -> GT.gmap bool subj)
              _fself_state
        end
  
      let gmap_state () subj =
        GT.gmap state_fuly ((fun () subj -> GT.gmap bool subj) ()) subj
      ;;
  
      let _ = gmap_state
  
      class ['extra_state] fmt_state_t _fself_state =
        object
          inherit [Format.formatter, 'extra_state, unit] state_t
          constraint 'extra_state = state
  
          inherit
            [bool, 'extra_state] fmt_state_fuly_t
              (fun inh___095_ subj___096_ -> GT.fmt bool inh___095_ subj___096_)
              _fself_state
        end
  
      let fmt_state inh___091_ subj___092_ =
        GT.fmt
          state_fuly
          (fun inh___093_ subj___094_ -> GT.fmt bool inh___093_ subj___094_)
          inh___091_
          subj___092_
      ;;
  
      let _ = fmt_state
  
      class ['extra_state] show_state_t _fself_state =
        object
          inherit [unit, 'extra_state, string] state_t
          constraint 'extra_state = state
  
          inherit
            [bool, 'extra_state] show_state_fuly_t
              (fun () subj -> GT.show bool subj)
              _fself_state
        end
  
      let show_state () subj =
        GT.show state_fuly ((fun () subj -> GT.show bool subj) ()) subj
      ;;
  
      let _ = show_state
  
      let state =
        { GT.fix = (fun eta -> GT.transform_gc gcata_state eta)
        ; GT.plugins =
            object
              method gmap subj = gmap_state () subj
              method fmt = fmt_state
              method show subj = show_state () subj
            end
        ; GT.gcata = gcata_state
        }
      ;;
  
      let _ = state
      let gmap_state subj = gmap_state () subj
      let _ = gmap_state
      let show_state subj = show_state () subj
      let _ = show_state
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type state_logic = bool OCanren.logic state_fuly OCanren.logic
    [@@deriving gt ~options:{ show; fmt; gmap }]
  
    include struct
      let _ = fun (_ : state_logic) -> ()
  
      class virtual ['inh, 'extra, 'syn] state_logic_t =
        object
          inherit
            [ bool OCanren.logic state_fuly
            , bool OCanren.logic state_fuly
            , bool OCanren.logic state_fuly
            , 'inh
            , 'extra
            , 'syn ]
            OCanren.logic_t
        end
  
      let gcata_state_logic = OCanren.gcata_logic
      let _ = gcata_state_logic
  
      class ['extra_state_logic, 'syn_state_logic] gmap_state_logic_t _fself_state_logic =
        object
          inherit [unit, 'extra_state_logic, state_logic] state_logic_t
          constraint 'extra_state_logic = state_logic
          constraint 'syn_state_logic = state_logic
  
          inherit
            [ bool OCanren.logic state_fuly
            , bool OCanren.logic state_fuly
            , 'extra_state_logic
            , 'syn_state_logic ]
            OCanren.gmap_logic_t
              (fun () subj ->
                GT.gmap
                  state_fuly
                  ((fun () subj ->
                     GT.gmap OCanren.logic ((fun () subj -> GT.gmap bool subj) ()) subj)
                     ())
                  subj)
              _fself_state_logic
        end
  
      let gmap_state_logic () subj =
        GT.gmap
          OCanren.logic
          ((fun () subj ->
             GT.gmap
               state_fuly
               ((fun () subj ->
                  GT.gmap OCanren.logic ((fun () subj -> GT.gmap bool subj) ()) subj)
                  ())
               subj)
             ())
          subj
      ;;
  
      let _ = gmap_state_logic
  
      class ['extra_state_logic] fmt_state_logic_t _fself_state_logic =
        object
          inherit [Format.formatter, 'extra_state_logic, unit] state_logic_t
          constraint 'extra_state_logic = state_logic
  
          inherit
            [bool OCanren.logic state_fuly, 'extra_state_logic] OCanren.fmt_logic_t
              (fun inh___105_ subj___106_ ->
                GT.fmt
                  state_fuly
                  (fun inh___107_ subj___108_ ->
                    GT.fmt
                      OCanren.logic
                      (fun inh___109_ subj___110_ -> GT.fmt bool inh___109_ subj___110_)
                      inh___107_
                      subj___108_)
                  inh___105_
                  subj___106_)
              _fself_state_logic
        end
  
      let fmt_state_logic inh___097_ subj___098_ =
        GT.fmt
          OCanren.logic
          (fun inh___099_ subj___100_ ->
            GT.fmt
              state_fuly
              (fun inh___101_ subj___102_ ->
                GT.fmt
                  OCanren.logic
                  (fun inh___103_ subj___104_ -> GT.fmt bool inh___103_ subj___104_)
                  inh___101_
                  subj___102_)
              inh___099_
              subj___100_)
          inh___097_
          subj___098_
      ;;
  
      let _ = fmt_state_logic
  
      class ['extra_state_logic] show_state_logic_t _fself_state_logic =
        object
          inherit [unit, 'extra_state_logic, string] state_logic_t
          constraint 'extra_state_logic = state_logic
  
          inherit
            [bool OCanren.logic state_fuly, 'extra_state_logic] OCanren.show_logic_t
              (fun () subj ->
                GT.show
                  state_fuly
                  ((fun () subj ->
                     GT.show OCanren.logic ((fun () subj -> GT.show bool subj) ()) subj)
                     ())
                  subj)
              _fself_state_logic
        end
  
      let show_state_logic () subj =
        GT.show
          OCanren.logic
          ((fun () subj ->
             GT.show
               state_fuly
               ((fun () subj ->
                  GT.show OCanren.logic ((fun () subj -> GT.show bool subj) ()) subj)
                  ())
               subj)
             ())
          subj
      ;;
  
      let _ = show_state_logic
  
      let state_logic =
        { GT.fix = (fun eta -> GT.transform_gc gcata_state_logic eta)
        ; GT.plugins =
            object
              method gmap subj = gmap_state_logic () subj
              method fmt = fmt_state_logic
              method show subj = show_state_logic () subj
            end
        ; GT.gcata = gcata_state_logic
        }
      ;;
  
      let _ = state_logic
      let gmap_state_logic subj = gmap_state_logic () subj
      let _ = gmap_state_logic
      let show_state_logic subj = show_state_logic () subj
      let _ = show_state_logic
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type state_injected = bool OCanren.ilogic state_fuly OCanren.ilogic
  
    let state_fmapt f__067_ subj__068_ =
      let open OCanren.Env.Monad in
      OCanren.Env.Monad.return (GT.gmap state_fuly) <*> f__067_ <*> subj__068_
    ;;
  
    let (state_prj_exn : (state_injected, state) OCanren.Reifier.t) =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun self ->
        OCanren.prj_exn <..> chain (state_fmapt OCanren.prj_exn))
    ;;
  
    let (state_reify : (state_injected, state_logic) OCanren.Reifier.t) =
      let open OCanren.Env.Monad in
      OCanren.Reifier.fix (fun self ->
        OCanren.reify
        <..> chain
               (OCanren.Reifier.zed
                  (OCanren.Reifier.rework ~fv:(state_fmapt OCanren.reify))))
    ;;
  end
  
  let rec greater a0 b0 q114 =
    a0
    === !!O
    &&& (q114 === !!false)
    ||| Fresh.one (fun x ->
      delay (fun () ->
        ?&[ a0 === !!(S x)
          ; b0
            === !!O
            &&& (q114 === !!true)
            ||| Fresh.one (fun y ->
              delay (fun () -> ?&[ b0 === !!(S y); greater x y q114 ]))
          ]))
  ;;
  
  let grForPerson x y q97 =
    conde
      [ x
        === !!A
        &&& conde
              [ y === !!A &&& (q97 === !!false)
              ; y === !!B &&& (q97 === !!true)
              ; y === !!C &&& (q97 === !!true)
              ; y === !!D &&& (q97 === !!true)
              ]
      ; x
        === !!B
        &&& conde
              [ y === !!A &&& (q97 === !!false)
              ; y === !!B &&& (q97 === !!false)
              ; y === !!C &&& (q97 === !!false)
              ; y === !!D &&& (q97 === !!true)
              ]
      ; x
        === !!C
        &&& conde
              [ y === !!A &&& (q97 === !!false)
              ; y === !!B &&& (q97 === !!false)
              ; y === !!C &&& (q97 === !!false)
              ; y === !!D &&& (q97 === !!true)
              ]
      ; x === !!D &&& (q97 === !!false)
      ]
  ;;
  
  let max a0 b0 q93 =
    Fresh.one (fun q94 ->
      delay (fun () ->
        ?&[ greater a0 b0 q94
          ; conde [ q94 === !!true &&& (a0 === q93); q94 === !!false &&& (b0 === q93) ]
          ]))
  ;;
  
  let rec add a0 b0 q91 =
    a0
    === !!O
    &&& (b0 === q91)
    ||| Fresh.one (fun x -> delay (fun () -> ?&[ a0 === !!(S x); add x !!(S b0) q91 ]))
  ;;
  
  let checkPerson state person q77 =
    Fresh.one (fun l ->
      Fresh.one (fun a0 ->
        Fresh.one (fun b0 ->
          Fresh.one (fun c0 ->
            Fresh.one (fun d0 ->
              delay (fun () ->
                ?&[ state === !!(St (l, a0, b0, c0, d0))
                  ; conde
                      [ person
                        === !!A
                        &&& conde
                              [ a0 === l &&& (q77 === !!true)
                              ; q77 === !!false &&& (a0 =/= l)
                              ]
                      ; person
                        === !!B
                        &&& conde
                              [ b0 === l &&& (q77 === !!true)
                              ; q77 === !!false &&& (b0 =/= l)
                              ]
                      ; person
                        === !!C
                        &&& conde
                              [ c0 === l &&& (q77 === !!true)
                              ; q77 === !!false &&& (c0 =/= l)
                              ]
                      ; person
                        === !!D
                        &&& conde
                              [ d0 === l &&& (q77 === !!true)
                              ; q77 === !!false &&& (d0 =/= l)
                              ]
                      ]
                  ]))))))
  ;;
  
  let checkStep state step q64 =
    Fresh.one (fun p -> delay (fun () -> ?&[ step === !!(One p); checkPerson state p q64 ]))
    ||| Fresh.one (fun p ->
      Fresh.one (fun q ->
        Fresh.one (fun q65 ->
          Fresh.one (fun q66 ->
            Fresh.one (fun q71 ->
              Fresh.one (fun q72 ->
                delay (fun () ->
                  ?&[ step === !!(Two (p, q))
                    ; checkPerson state p q65
                    ; checkPerson state q q71
                    ; grForPerson p q q72
                    ; conde
                        [ q71 === !!false &&& (q66 === !!false)
                        ; q71 === !!true &&& (q66 === q72)
                        ]
                    ; conde
                        [ q65 === !!false &&& (q64 === !!false)
                        ; q65 === !!true &&& (q64 === q66)
                        ]
                    ])))))))
  ;;
  
  let moveLight state q59 =
    Fresh.one (fun l ->
      Fresh.one (fun a0 ->
        Fresh.one (fun b0 ->
          Fresh.one (fun c0 ->
            Fresh.one (fun d0 ->
              Fresh.one (fun q60 ->
                delay (fun () ->
                  ?&[ state === !!(St (l, a0, b0, c0, d0))
                    ; q59 === !!(St (q60, a0, b0, c0, d0))
                    ; conde
                        [ l === !!true &&& (q60 === !!false)
                        ; l === !!false &&& (q60 === !!true)
                        ]
                    ])))))))
  ;;
  
  let movePerson state person q41 =
    Fresh.one (fun l ->
      Fresh.one (fun a0 ->
        Fresh.one (fun b0 ->
          Fresh.one (fun c0 ->
            Fresh.one (fun d0 ->
              delay (fun () ->
                ?&[ state === !!(St (l, a0, b0, c0, d0))
                  ; conde
                      [ Fresh.one (fun q43 ->
                          delay (fun () ->
                            ?&[ person === !!A
                              ; q41 === !!(St (l, q43, b0, c0, d0))
                              ; conde
                                  [ a0 === !!true &&& (q43 === !!false)
                                  ; a0 === !!false &&& (q43 === !!true)
                                  ]
                              ]))
                      ; Fresh.one (fun q47 ->
                          delay (fun () ->
                            ?&[ person === !!B
                              ; q41 === !!(St (l, a0, q47, c0, d0))
                              ; conde
                                  [ b0 === !!true &&& (q47 === !!false)
                                  ; b0 === !!false &&& (q47 === !!true)
                                  ]
                              ]))
                      ; Fresh.one (fun q51 ->
                          delay (fun () ->
                            ?&[ person === !!C
                              ; q41 === !!(St (l, a0, b0, q51, d0))
                              ; conde
                                  [ c0 === !!true &&& (q51 === !!false)
                                  ; c0 === !!false &&& (q51 === !!true)
                                  ]
                              ]))
                      ; Fresh.one (fun q55 ->
                          delay (fun () ->
                            ?&[ person === !!D
                              ; q41 === !!(St (l, a0, b0, c0, q55))
                              ; conde
                                  [ d0 === !!true &&& (q55 === !!false)
                                  ; d0 === !!false &&& (q55 === !!true)
                                  ]
                              ]))
                      ]
                  ]))))))
  ;;
  
  let step state step q34 =
    Fresh.one (fun p ->
      Fresh.one (fun q35 ->
        delay (fun () ->
          ?&[ step === !!(One p); movePerson state p q35; moveLight q35 q34 ])))
    ||| Fresh.one (fun p ->
      Fresh.one (fun q ->
        Fresh.one (fun q37 ->
          Fresh.one (fun q39 ->
            delay (fun () ->
              ?&[ step === !!(Two (p, q))
                ; movePerson state p q39
                ; movePerson q39 q q37
                ; moveLight q37 q34
                ])))))
  ;;
  
  let times p q29 =
    conde
      [ p === !!A &&& (q29 === !!(S !!O))
      ; p === !!B &&& (q29 === !!(S !!(S !!O)))
      ; p === !!C &&& (q29 === !!(S !!(S !!(S !!(S !!(S !!O))))))
      ; p
        === !!D
        &&& (q29 === !!(S !!(S !!(S !!(S !!(S !!(S !!(S !!(S !!(S !!(S !!O)))))))))))
      ]
  ;;
  
  let getTime state q25 =
    Fresh.one (fun p -> delay (fun () -> ?&[ state === !!(One p); times p q25 ]))
    ||| Fresh.one (fun p ->
      Fresh.one (fun q ->
        Fresh.one (fun q26 ->
          Fresh.one (fun q27 ->
            delay (fun () ->
              ?&[ state === !!(Two (p, q)); times p q26; times q q27; max q26 q27 q25 ])))))
  ;;
  
  let rec getAnswerInner answer state finish q4 =
    Fresh.one (fun x ->
      Fresh.one (fun xs ->
        Fresh.one (fun q6 ->
          delay (fun () ->
            ?&[ answer === !!(OCanren.Std.List.Cons (x, xs))
              ; checkStep state x q6
              ; conde
                  [ Fresh.one (fun q8 ->
                      Fresh.one (fun q14 ->
                        delay (fun () ->
                          ?&[ q6 === !!true
                            ; step state x q14
                            ; getAnswerInner xs q14 finish q8
                            ; q8
                              === !!None
                              &&& (q4 === !!None)
                              ||| Fresh.one (fun t1 ->
                                Fresh.one (fun q10 ->
                                  Fresh.one (fun q12 ->
                                    delay (fun () ->
                                      ?&[ q8 === !!(Some t1)
                                        ; q4 === !!(Some q10)
                                        ; getTime x q12
                                        ; add q12 t1 q10
                                        ]))))
                            ])))
                  ; q6 === !!false &&& (q4 === !!None)
                  ]
              ]))))
    ||| Fresh.one (fun q18 ->
      delay (fun () ->
        ?&[ answer === !!OCanren.Std.List.Nil
          ; conde
              [ state === finish &&& (q18 === !!true)
              ; q18 === !!false &&& (state =/= finish)
              ]
          ; conde
              [ q18 === !!true &&& (q4 === !!(Some !!O))
              ; q18 === !!false &&& (q4 === !!None)
              ]
          ]))
  ;;
  
  let getAnswer answer q1 =
    Fresh.one (fun start ->
      Fresh.one (fun finish ->
        delay (fun () ->
          ?&[ start === !!(St (!!true, !!true, !!true, !!true, !!true))
            ; finish === !!(St (!!false, !!false, !!false, !!false, !!false))
            ; getAnswerInner answer start finish q1
            ])))
  ;;
  
  let rec checkPerson_2true state person =
    conde
      [ Fresh.five (fun l a0 b0 c0 d0 ->
          state === !!(St (l, a0, b0, c0, d0)) &&& (person === !!A &&& (a0 === l)))
      ; Fresh.five (fun l a0 b0 c0 d0 ->
          state === !!(St (l, a0, b0, c0, d0)) &&& (person === !!B &&& (b0 === l)))
      ; Fresh.five (fun l a0 b0 c0 d0 ->
          state === !!(St (l, a0, b0, c0, d0)) &&& (person === !!C &&& (c0 === l)))
      ; Fresh.five (fun l a0 b0 c0 d0 ->
          state === !!(St (l, a0, b0, c0, d0)) &&& (person === !!D &&& (d0 === l)))
      ]
  
  and checkPerson_2false state person =
    conde
      [ Fresh.five (fun l a0 b0 c0 d0 ->
          state === !!(St (l, a0, b0, c0, d0)) &&& (person === !!A &&& (a0 =/= l)))
      ; Fresh.five (fun l a0 b0 c0 d0 ->
          state === !!(St (l, a0, b0, c0, d0)) &&& (person === !!B &&& (b0 =/= l)))
      ; Fresh.five (fun l a0 b0 c0 d0 ->
          state === !!(St (l, a0, b0, c0, d0)) &&& (person === !!C &&& (c0 =/= l)))
      ; Fresh.five (fun l a0 b0 c0 d0 ->
          state === !!(St (l, a0, b0, c0, d0)) &&& (person === !!D &&& (d0 =/= l)))
      ]
  
  and grForPerson_2true x y =
    conde
      [ x === !!A &&& (y === !!B)
      ; x === !!A &&& (y === !!C)
      ; x === !!A &&& (y === !!D)
      ; x === !!B &&& (y === !!D)
      ; x === !!C &&& (y === !!D)
      ]
  
  and movePerson_0St constarg0 constarg1 constarg2 constarg3 constarg4 person q41 =
    conde
      [ constarg1
        === !!true
        &&& (person
             === !!A
             &&& (q41 === !!(St (constarg0, !!false, constarg2, constarg3, constarg4))))
      ; constarg1
        === !!false
        &&& (person
             === !!A
             &&& (q41 === !!(St (constarg0, !!true, constarg2, constarg3, constarg4))))
      ; constarg2
        === !!true
        &&& (person
             === !!B
             &&& (q41 === !!(St (constarg0, constarg1, !!false, constarg3, constarg4))))
      ; constarg2
        === !!false
        &&& (person
             === !!B
             &&& (q41 === !!(St (constarg0, constarg1, !!true, constarg3, constarg4))))
      ; constarg3
        === !!true
        &&& (person
             === !!C
             &&& (q41 === !!(St (constarg0, constarg1, constarg2, !!false, constarg4))))
      ; constarg3
        === !!false
        &&& (person
             === !!C
             &&& (q41 === !!(St (constarg0, constarg1, constarg2, !!true, constarg4))))
      ; constarg4
        === !!true
        &&& (person
             === !!D
             &&& (q41 === !!(St (constarg0, constarg1, constarg2, constarg3, !!false))))
      ; constarg4
        === !!false
        &&& (person
             === !!D
             &&& (q41 === !!(St (constarg0, constarg1, constarg2, constarg3, !!true))))
      ]
  
  and checkStep_2false state step =
    conde
      [ Fresh.one (fun p -> step === !!(One p) &&& checkPerson_2false state p)
      ; Fresh.three (fun p q q72 ->
          step
          === !!(Two (p, q))
          &&& (checkPerson_2false state p
               &&& (checkPerson_2false state q &&& grForPerson p q q72)))
      ; Fresh.three (fun p q q72 ->
          step
          === !!(Two (p, q))
          &&& (checkPerson_2true state p
               &&& (checkPerson_2false state q &&& grForPerson p q q72)))
      ; Fresh.four (fun p q q66 q72 ->
          step
          === !!(Two (p, q))
          &&& (checkPerson_2false state p
               &&& (checkPerson_2true state q &&& (grForPerson p q q72 &&& (q66 === q72)))))
      ; Fresh.two (fun p q ->
          step
          === !!(Two (p, q))
          &&& (checkPerson_2true state p
               &&& (checkPerson_2true state q &&& grForPerson_2false p q)))
      ]
  
  and checkStep_2true state step =
    Fresh.one (fun p -> step === !!(One p) &&& checkPerson_2true state p)
    ||| Fresh.two (fun p q ->
      step
      === !!(Two (p, q))
      &&& (checkPerson_2true state p
           &&& (checkPerson_2true state q &&& grForPerson_2true p q)))
  
  and grForPerson_2false x y =
    conde
      [ x === !!A &&& (y === !!A)
      ; x === !!B &&& (y === !!A)
      ; x === !!B &&& (y === !!B)
      ; x === !!B &&& (y === !!C)
      ; x === !!C &&& (y === !!A)
      ; x === !!C &&& (y === !!B)
      ; x === !!C &&& (y === !!C)
      ; x === !!D
      ]
  
  and checkPerson_0St_2true constarg0 constarg1 constarg2 constarg3 constarg4 person =
    conde
      [ constarg1 === constarg0 &&& (person === !!A)
      ; constarg2 === constarg0 &&& (person === !!B)
      ; constarg3 === constarg0 &&& (person === !!C)
      ; constarg4 === constarg0 &&& (person === !!D)
      ]
  
  and checkPerson_0St_2false constarg0 constarg1 constarg2 constarg3 constarg4 person =
    conde
      [ person === !!A &&& (constarg1 =/= constarg0)
      ; person === !!B &&& (constarg2 =/= constarg0)
      ; person === !!C &&& (constarg3 =/= constarg0)
      ; person === !!D &&& (constarg4 =/= constarg0)
      ]
  
  and checkStep_0St_2false constarg0 constarg1 constarg2 constarg3 constarg4 step =
    conde
      [ Fresh.one (fun p ->
          step
          === !!(One p)
          &&& checkPerson_0St_2false constarg0 constarg1 constarg2 constarg3 constarg4 p)
      ; Fresh.three (fun p q q72 ->
          step
          === !!(Two (p, q))
          &&& (checkPerson_0St_2false constarg0 constarg1 constarg2 constarg3 constarg4 p
               &&& (checkPerson_0St_2false
                      constarg0
                      constarg1
                      constarg2
                      constarg3
                      constarg4
                      q
                    &&& grForPerson p q q72)))
      ; Fresh.three (fun p q q72 ->
          step
          === !!(Two (p, q))
          &&& (checkPerson_0St_2true constarg0 constarg1 constarg2 constarg3 constarg4 p
               &&& (checkPerson_0St_2false
                      constarg0
                      constarg1
                      constarg2
                      constarg3
                      constarg4
                      q
                    &&& grForPerson p q q72)))
      ; Fresh.four (fun p q q66 q72 ->
          step
          === !!(Two (p, q))
          &&& (checkPerson_0St_2false constarg0 constarg1 constarg2 constarg3 constarg4 p
               &&& (checkPerson_0St_2true
                      constarg0
                      constarg1
                      constarg2
                      constarg3
                      constarg4
                      q
                    &&& (grForPerson p q q72 &&& (q66 === q72)))))
      ; Fresh.two (fun p q ->
          step
          === !!(Two (p, q))
          &&& (checkPerson_0St_2true constarg0 constarg1 constarg2 constarg3 constarg4 p
               &&& (checkPerson_0St_2true
                      constarg0
                      constarg1
                      constarg2
                      constarg3
                      constarg4
                      q
                    &&& grForPerson_2false p q)))
      ]
  
  and getAnswerInner_2St_3None
    answer
    state
    constarg0
    constarg1
    constarg2
    constarg3
    constarg4
    =
    conde
      [ Fresh.three (fun x xs q14 ->
          answer
          === !!(Cons (x, xs))
          &&& (checkStep_2true state x
               &&& (step state x q14
                    &&& getAnswerInner_2St_3None
                          xs
                          q14
                          constarg0
                          constarg1
                          constarg2
                          constarg3
                          constarg4)))
      ; Fresh.two (fun x xs -> answer === !!(Cons (x, xs)) &&& checkStep_2false state x)
      ; answer
        === !!Nil
        &&& (state =/= !!(St (constarg0, constarg1, constarg2, constarg3, constarg4)))
      ]
  
  and getAnswerInner_2St_3Some
    answer
    state
    constarg0
    constarg1
    constarg2
    constarg3
    constarg4
    constarg5
    =
    Fresh.five (fun x xs q14 t1 q12 ->
      answer
      === !!(Cons (x, xs))
      &&& (checkStep_2true state x
           &&& (step state x q14
                &&& (getAnswerInner_2St_3Some
                       xs
                       q14
                       constarg0
                       constarg1
                       constarg2
                       constarg3
                       constarg4
                       t1
                     &&& (getTime x q12 &&& add q12 t1 constarg5)))))
    ||| (answer
         === !!Nil
         &&& (state
              === !!(St (constarg0, constarg1, constarg2, constarg3, constarg4))
              &&& (constarg5 === !!O)))
  
  and step_0St constarg0 constarg1 constarg2 constarg3 constarg4 step q34 =
    Fresh.two (fun p q35 ->
      step
      === !!(One p)
      &&& (movePerson_0St constarg0 constarg1 constarg2 constarg3 constarg4 p q35
           &&& moveLight q35 q34))
    ||| Fresh.four (fun p q q37 q39 ->
      step
      === !!(Two (p, q))
      &&& (movePerson_0St constarg0 constarg1 constarg2 constarg3 constarg4 p q39
           &&& (movePerson q39 q q37 &&& moveLight q37 q34)))
  
  and checkStep_0St_2true constarg0 constarg1 constarg2 constarg3 constarg4 step =
    Fresh.one (fun p ->
      step
      === !!(One p)
      &&& checkPerson_0St_2true constarg0 constarg1 constarg2 constarg3 constarg4 p)
    ||| Fresh.two (fun p q ->
      step
      === !!(Two (p, q))
      &&& (checkPerson_0St_2true constarg0 constarg1 constarg2 constarg3 constarg4 p
           &&& (checkPerson_0St_2true constarg0 constarg1 constarg2 constarg3 constarg4 q
                &&& grForPerson_2true p q)))
  
  and getAnswerInner_1St_2St_3Some
    answer
    constarg0
    constarg1
    constarg2
    constarg3
    constarg4
    constarg5
    constarg6
    constarg7
    constarg8
    constarg9
    constarg10
    =
    Fresh.five (fun x xs q14 t1 q12 ->
      answer
      === !!(Cons (x, xs))
      &&& (checkStep_0St_2true constarg0 constarg1 constarg2 constarg3 constarg4 x
           &&& (step_0St constarg0 constarg1 constarg2 constarg3 constarg4 x q14
                &&& (getAnswerInner_2St_3Some
                       xs
                       q14
                       constarg5
                       constarg6
                       constarg7
                       constarg8
                       constarg9
                       t1
                     &&& (getTime x q12 &&& add q12 t1 constarg10)))))
    ||| (answer
         === !!Nil
         &&& (constarg5
              === constarg0
              &&& (constarg6
                   === constarg1
                   &&& (constarg7
                        === constarg2
                        &&& (constarg8
                             === constarg3
                             &&& (constarg9 === constarg4 &&& (constarg10 === !!O)))))))
  
  and getAnswerInner_1St_2St_3None
    answer
    constarg0
    constarg1
    constarg2
    constarg3
    constarg4
    constarg5
    constarg6
    constarg7
    constarg8
    constarg9
    =
    conde
      [ Fresh.three (fun x xs q14 ->
          answer
          === !!(Cons (x, xs))
          &&& (checkStep_0St_2true constarg0 constarg1 constarg2 constarg3 constarg4 x
               &&& (step_0St constarg0 constarg1 constarg2 constarg3 constarg4 x q14
                    &&& getAnswerInner_2St_3None
                          xs
                          q14
                          constarg5
                          constarg6
                          constarg7
                          constarg8
                          constarg9)))
      ; Fresh.two (fun x xs ->
          answer
          === !!(Cons (x, xs))
          &&& checkStep_0St_2false constarg0 constarg1 constarg2 constarg3 constarg4 x)
      ; answer
        === !!Nil
        &&& (constarg5
             =/= constarg0
             &&& (constarg6
                  =/= constarg1
                  &&& (constarg7
                       =/= constarg2
                       &&& (constarg8 =/= constarg3 &&& (constarg9 =/= constarg4)))))
      ]
  
  and getAnswer_1None answer =
    getAnswerInner_1St_2St_3None
      answer
      !!true
      !!true
      !!true
      !!true
      !!true
      !!false
      !!false
      !!false
      !!false
      !!false
  
  and getAnswer_1Some answer constarg0 =
    getAnswerInner_1St_2St_3Some
      answer
      !!true
      !!true
      !!true
      !!true
      !!true
      !!false
      !!false
      !!false
      !!false
      !!false
      constarg0
  ;;
