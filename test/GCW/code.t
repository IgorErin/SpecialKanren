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
    type nonrec person_fuly =
      | G
      | C
      | W
      | N
    [@@deriving gt ~options:{ gmap; show }]
  
    include struct
      let _ = fun (_ : person_fuly) -> ()
  
      class virtual ['inh, 'extra, 'syn] person_fuly_t =
        object
          method virtual c_G : 'inh -> 'extra -> 'syn
          method virtual c_C : 'inh -> 'extra -> 'syn
          method virtual c_W : 'inh -> 'extra -> 'syn
          method virtual c_N : 'inh -> 'extra -> 'syn
        end
  
      let gcata_person_fuly (tr : (_, person_fuly, _) #person_fuly_t) inh subj =
        match subj with
        | G -> tr#c_G inh subj
        | C -> tr#c_C inh subj
        | W -> tr#c_W inh subj
        | N -> tr#c_N inh subj
      ;;
  
      let _ = gcata_person_fuly
  
      class ['extra_person_fuly] show_person_fuly_t _fself_person_fuly =
        object
          inherit [unit, 'extra_person_fuly, string] person_fuly_t
          constraint 'extra_person_fuly = person_fuly
  
          method c_G () _ =
            match () with
            | () -> "G"
  
          method c_C () _ =
            match () with
            | () -> "C"
  
          method c_W () _ =
            match () with
            | () -> "W"
  
          method c_N () _ =
            match () with
            | () -> "N"
        end
  
      let show_person_fuly inh0 subj =
        GT.transform_gc gcata_person_fuly (new show_person_fuly_t) inh0 subj
      ;;
  
      let _ = show_person_fuly
  
      class ['extra_person_fuly, 'syn_person_fuly] gmap_person_fuly_t _fself_person_fuly =
        object
          inherit [unit, 'extra_person_fuly, person_fuly] person_fuly_t
          constraint 'extra_person_fuly = person_fuly
          constraint 'syn_person_fuly = person_fuly
          method c_G () _ = G
          method c_C () _ = C
          method c_W () _ = W
          method c_N () _ = N
        end
  
      let gmap_person_fuly inh0 subj =
        GT.transform_gc gcata_person_fuly (new gmap_person_fuly_t) inh0 subj
      ;;
  
      let _ = gmap_person_fuly
  
      let person_fuly =
        { GT.fix = (fun eta -> GT.transform_gc gcata_person_fuly eta)
        ; GT.plugins =
            object
              method show subj = show_person_fuly () subj
              method gmap subj = gmap_person_fuly () subj
            end
        ; GT.gcata = gcata_person_fuly
        }
      ;;
  
      let _ = person_fuly
      let show_person_fuly subj = show_person_fuly () subj
      let _ = show_person_fuly
      let gmap_person_fuly subj = gmap_person_fuly () subj
      let _ = gmap_person_fuly
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type person = person_fuly [@@deriving gt ~options:{ gmap; show }]
  
    include struct
      let _ = fun (_ : person) -> ()
  
      class virtual ['inh, 'extra, 'syn] person_t =
        object
          inherit ['inh, 'extra, 'syn] person_fuly_t
        end
  
      let gcata_person = gcata_person_fuly
      let _ = gcata_person
  
      class ['extra_person] show_person_t _fself_person =
        object
          inherit [unit, 'extra_person, string] person_t
          constraint 'extra_person = person
          inherit ['extra_person] show_person_fuly_t _fself_person
        end
  
      let show_person () subj = GT.show person_fuly subj
      let _ = show_person
  
      class ['extra_person, 'syn_person] gmap_person_t _fself_person =
        object
          inherit [unit, 'extra_person, person] person_t
          constraint 'extra_person = person
          constraint 'syn_person = person
          inherit ['extra_person, 'syn_person] gmap_person_fuly_t _fself_person
        end
  
      let gmap_person () subj = GT.gmap person_fuly subj
      let _ = gmap_person
  
      let person =
        { GT.fix = (fun eta -> GT.transform_gc gcata_person eta)
        ; GT.plugins =
            object
              method show subj = show_person () subj
              method gmap subj = gmap_person () subj
            end
        ; GT.gcata = gcata_person
        }
      ;;
  
      let _ = person
      let show_person subj = show_person () subj
      let _ = show_person
      let gmap_person subj = gmap_person () subj
      let _ = gmap_person
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type person_logic = person_fuly OCanren.logic [@@deriving gt ~options:{ gmap; show }]
  
    include struct
      let _ = fun (_ : person_logic) -> ()
  
      class virtual ['inh, 'extra, 'syn] person_logic_t =
        object
          inherit
            [person_fuly, person_fuly, person_fuly, 'inh, 'extra, 'syn] OCanren.logic_t
        end
  
      let gcata_person_logic = OCanren.gcata_logic
      let _ = gcata_person_logic
  
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
  
      let person_logic =
        { GT.fix = (fun eta -> GT.transform_gc gcata_person_logic eta)
        ; GT.plugins =
            object
              method show subj = show_person_logic () subj
              method gmap subj = gmap_person_logic () subj
            end
        ; GT.gcata = gcata_person_logic
        }
      ;;
  
      let _ = person_logic
      let show_person_logic subj = show_person_logic () subj
      let _ = show_person_logic
      let gmap_person_logic subj = gmap_person_logic () subj
      let _ = gmap_person_logic
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type person_injected = person_fuly OCanren.ilogic
  
    let person_fmapt subj__001_ =
      let open OCanren.Env.Monad in
      OCanren.Env.Monad.return (GT.gmap person_fuly) <*> subj__001_
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
    type nonrec 'a0 state_fuly = St of 'a0 * 'a0 * 'a0 * 'a0
    [@@deriving gt ~options:{ gmap; show }]
  
    include struct
      let _ = fun (_ : 'a0 state_fuly) -> ()
  
      class virtual ['ia0, 'a0, 'sa0, 'inh, 'extra, 'syn] state_fuly_t =
        object
          method virtual c_St : 'inh -> 'extra -> 'a0 -> 'a0 -> 'a0 -> 'a0 -> 'syn
        end
  
      let gcata_state_fuly
        (tr : (_, 'typ0__009_, _, _, 'typ0__009_ state_fuly, _) #state_fuly_t)
        inh
        subj
        =
        match subj with
        | St (_x__005_, _x__006_, _x__007_, _x__008_) ->
          tr#c_St inh subj _x__005_ _x__006_ _x__007_ _x__008_
      ;;
  
      let _ = gcata_state_fuly
  
      class ['a0, 'extra_state_fuly] show_state_fuly_t fa0 _fself_state_fuly =
        object
          inherit [unit, 'a0, string, unit, 'extra_state_fuly, string] state_fuly_t
          constraint 'extra_state_fuly = 'a0 state_fuly
  
          method c_St () _ _x__010_ _x__011_ _x__012_ _x__013_ =
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
                                                   , CamlinternalFormatBasics.Char_literal
                                                       ( ')'
                                                       , CamlinternalFormatBasics
                                                         .End_of_format ) ) ) ) ) ) ) ) )
                   , "St (%s, %s, %s, %s)" ))
                (fa0 () _x__010_)
                (fa0 () _x__011_)
                (fa0 () _x__012_)
                (fa0 () _x__013_)
        end
  
      let show_state_fuly fa0 inh0 subj =
        GT.transform_gc gcata_state_fuly (new show_state_fuly_t fa0) inh0 subj
      ;;
  
      let _ = show_state_fuly
  
      class ['a0, 'a0_2, 'extra_state_fuly, 'syn_state_fuly] gmap_state_fuly_t
        fa0
        _fself_state_fuly =
        object
          inherit [unit, 'a0, 'a0_2, unit, 'extra_state_fuly, 'a0_2 state_fuly] state_fuly_t
          constraint 'extra_state_fuly = 'a0 state_fuly
          constraint 'syn_state_fuly = 'a0_2 state_fuly
  
          method c_St () _ _x__014_ _x__015_ _x__016_ _x__017_ =
            St (fa0 () _x__014_, fa0 () _x__015_, fa0 () _x__016_, fa0 () _x__017_)
        end
  
      let gmap_state_fuly fa0 inh0 subj =
        GT.transform_gc gcata_state_fuly (new gmap_state_fuly_t fa0) inh0 subj
      ;;
  
      let _ = gmap_state_fuly
  
      let state_fuly =
        { GT.fix = (fun eta -> GT.transform_gc gcata_state_fuly eta)
        ; GT.plugins =
            object
              method show fa0 subj = show_state_fuly (GT.lift fa0) () subj
              method gmap fa0 subj = gmap_state_fuly (GT.lift fa0) () subj
            end
        ; GT.gcata = gcata_state_fuly
        }
      ;;
  
      let _ = state_fuly
      let show_state_fuly fa0 subj = show_state_fuly (GT.lift fa0) () subj
      let _ = show_state_fuly
      let gmap_state_fuly fa0 subj = gmap_state_fuly (GT.lift fa0) () subj
      let _ = gmap_state_fuly
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type state = bool state_fuly [@@deriving gt ~options:{ gmap; show }]
  
    include struct
      let _ = fun (_ : state) -> ()
  
      class virtual ['inh, 'extra, 'syn] state_t =
        object
          inherit [bool, bool, bool, 'inh, 'extra, 'syn] state_fuly_t
        end
  
      let gcata_state = gcata_state_fuly
      let _ = gcata_state
  
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
  
      let state =
        { GT.fix = (fun eta -> GT.transform_gc gcata_state eta)
        ; GT.plugins =
            object
              method show subj = show_state () subj
              method gmap subj = gmap_state () subj
            end
        ; GT.gcata = gcata_state
        }
      ;;
  
      let _ = state
      let show_state subj = show_state () subj
      let _ = show_state
      let gmap_state subj = gmap_state () subj
      let _ = gmap_state
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type state_logic = bool OCanren.logic state_fuly OCanren.logic
    [@@deriving gt ~options:{ gmap; show }]
  
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
  
      let state_logic =
        { GT.fix = (fun eta -> GT.transform_gc gcata_state_logic eta)
        ; GT.plugins =
            object
              method show subj = show_state_logic () subj
              method gmap subj = gmap_state_logic () subj
            end
        ; GT.gcata = gcata_state_logic
        }
      ;;
  
      let _ = state_logic
      let show_state_logic subj = show_state_logic () subj
      let _ = show_state_logic
      let gmap_state_logic subj = gmap_state_logic () subj
      let _ = gmap_state_logic
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
    type state_injected = bool OCanren.ilogic state_fuly OCanren.ilogic
  
    let state_fmapt f__003_ subj__004_ =
      let open OCanren.Env.Monad in
      OCanren.Env.Monad.return (GT.gmap state_fuly) <*> f__003_ <*> subj__004_
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
  
  let checkState s q55 =
    Fresh.one (fun i0 ->
      Fresh.one (fun g0 ->
        Fresh.one (fun c0 ->
          Fresh.one (fun w0 ->
            Fresh.one (fun q57 ->
              delay (fun () ->
                ?&[ s === !!(St (i0, g0, c0, w0))
                  ; conde
                      [ i0 === g0 &&& (q57 === !!true); q57 === !!false &&& (i0 =/= g0) ]
                  ; conde
                      [ q57 === !!true &&& (q55 === !!true)
                      ; Fresh.one (fun q60 ->
                          delay (fun () ->
                            ?&[ q57 === !!false
                              ; conde
                                  [ i0 === c0 &&& (q60 === !!true)
                                  ; q60 === !!false &&& (i0 =/= c0)
                                  ]
                              ; conde
                                  [ q60
                                    === !!true
                                    &&& conde
                                          [ i0 === w0 &&& (q55 === !!true)
                                          ; q55 === !!false &&& (i0 =/= w0)
                                          ]
                                  ; q60 === !!false &&& (q55 === !!false)
                                  ]
                              ]))
                      ]
                  ]))))))
  ;;
  
  let checkStep state step q43 =
    Fresh.one (fun i0 ->
      Fresh.one (fun g0 ->
        Fresh.one (fun c0 ->
          Fresh.one (fun w0 ->
            delay (fun () ->
              ?&[ state === !!(St (i0, g0, c0, w0))
                ; conde
                    [ step === !!N &&& (q43 === !!true)
                    ; step
                      === !!G
                      &&& conde
                            [ i0 === g0 &&& (q43 === !!true)
                            ; q43 === !!false &&& (i0 =/= g0)
                            ]
                    ; step
                      === !!C
                      &&& conde
                            [ i0 === c0 &&& (q43 === !!true)
                            ; q43 === !!false &&& (i0 =/= c0)
                            ]
                    ; step
                      === !!W
                      &&& conde
                            [ i0 === w0 &&& (q43 === !!true)
                            ; q43 === !!false &&& (i0 =/= w0)
                            ]
                    ]
                ])))))
  ;;
  
  let step s p q16 =
    Fresh.one (fun i0 ->
      Fresh.one (fun g0 ->
        Fresh.one (fun c0 ->
          Fresh.one (fun w0 ->
            delay (fun () ->
              ?&[ s === !!(St (i0, g0, c0, w0))
                ; conde
                    [ Fresh.one (fun q18 ->
                        Fresh.one (fun q19 ->
                          delay (fun () ->
                            ?&[ p === !!G
                              ; q16 === !!(St (q18, q19, c0, w0))
                              ; conde
                                  [ i0 === !!true &&& (q18 === !!false)
                                  ; i0 === !!false &&& (q18 === !!true)
                                  ]
                              ; conde
                                  [ g0 === !!true &&& (q19 === !!false)
                                  ; g0 === !!false &&& (q19 === !!true)
                                  ]
                              ])))
                    ; Fresh.one (fun q25 ->
                        Fresh.one (fun q26 ->
                          delay (fun () ->
                            ?&[ p === !!C
                              ; q16 === !!(St (q25, g0, q26, w0))
                              ; conde
                                  [ i0 === !!true &&& (q25 === !!false)
                                  ; i0 === !!false &&& (q25 === !!true)
                                  ]
                              ; conde
                                  [ c0 === !!true &&& (q26 === !!false)
                                  ; c0 === !!false &&& (q26 === !!true)
                                  ]
                              ])))
                    ; Fresh.one (fun q32 ->
                        Fresh.one (fun q33 ->
                          delay (fun () ->
                            ?&[ p === !!W
                              ; q16 === !!(St (q32, g0, c0, q33))
                              ; conde
                                  [ i0 === !!true &&& (q32 === !!false)
                                  ; i0 === !!false &&& (q32 === !!true)
                                  ]
                              ; conde
                                  [ w0 === !!true &&& (q33 === !!false)
                                  ; w0 === !!false &&& (q33 === !!true)
                                  ]
                              ])))
                    ; Fresh.one (fun q39 ->
                        delay (fun () ->
                          ?&[ p === !!N
                            ; q16 === !!(St (q39, g0, c0, w0))
                            ; conde
                                [ i0 === !!true &&& (q39 === !!false)
                                ; i0 === !!false &&& (q39 === !!true)
                                ]
                            ]))
                    ]
                ])))))
  ;;
  
  let rec checkAnswerInner a state finishState q4 =
    a
    === !!OCanren.Std.List.Nil
    &&& conde
          [ state === finishState &&& (q4 === !!true)
          ; q4 === !!false &&& (state =/= finishState)
          ]
    ||| Fresh.one (fun x ->
      Fresh.one (fun xs ->
        Fresh.one (fun q9 ->
          delay (fun () ->
            ?&[ a === !!(OCanren.Std.List.Cons (x, xs))
              ; checkStep state x q9
              ; conde
                  [ Fresh.one (fun newState ->
                      Fresh.one (fun q12 ->
                        delay (fun () ->
                          ?&[ q9 === !!true
                            ; step state x newState
                            ; checkState newState q12
                            ; conde
                                [ q12
                                  === !!true
                                  &&& checkAnswerInner xs newState finishState q4
                                ; q12 === !!false &&& (q4 === !!false)
                                ]
                            ])))
                  ; q9 === !!false &&& (q4 === !!false)
                  ]
              ]))))
  ;;
  
  let checkAnswer a q1 =
    Fresh.one (fun startState ->
      Fresh.one (fun finishState ->
        delay (fun () ->
          ?&[ startState === !!(St (!!true, !!true, !!true, !!true))
            ; finishState === !!(St (!!false, !!false, !!false, !!false))
            ; checkAnswerInner a startState finishState q1
            ])))
  ;;
  
  let rec checkStep_false state step =
    conde
      [ Fresh.four (fun i0 g0 c0 w0 ->
          state === !!(St (i0, g0, c0, w0)) &&& (step === !!G &&& (i0 =/= g0)))
      ; Fresh.four (fun i0 g0 c0 w0 ->
          state === !!(St (i0, g0, c0, w0)) &&& (step === !!C &&& (i0 =/= c0)))
      ; Fresh.four (fun i0 g0 c0 w0 ->
          state === !!(St (i0, g0, c0, w0)) &&& (step === !!W &&& (i0 =/= w0)))
      ]
  
  and checkStep_true state step =
    conde
      [ Fresh.four (fun i0 g0 c0 w0 -> state === !!(St (i0, g0, c0, w0)) &&& (step === !!N))
      ; Fresh.four (fun i0 g0 c0 w0 ->
          state === !!(St (i0, g0, c0, w0)) &&& (step === !!G &&& (i0 === g0)))
      ; Fresh.four (fun i0 g0 c0 w0 ->
          state === !!(St (i0, g0, c0, w0)) &&& (step === !!C &&& (i0 === c0)))
      ; Fresh.four (fun i0 g0 c0 w0 ->
          state === !!(St (i0, g0, c0, w0)) &&& (step === !!W &&& (i0 === w0)))
      ]
  
  and checkStep_St_false constarg0 constarg1 constarg2 constarg3 step =
    conde
      [ step === !!G &&& (constarg0 =/= constarg1)
      ; step === !!C &&& (constarg0 =/= constarg2)
      ; step === !!W &&& (constarg0 =/= constarg3)
      ]
  
  and checkState_false s =
    Fresh.four (fun i0 g0 c0 w0 ->
      s === !!(St (i0, g0, c0, w0)) &&& (i0 =/= g0 &&& (i0 === c0 &&& (i0 =/= w0))))
    ||| Fresh.four (fun i0 g0 c0 w0 ->
      s === !!(St (i0, g0, c0, w0)) &&& (i0 =/= g0 &&& (i0 =/= c0)))
  
  and checkAnswerInner_St_false a state constarg0 constarg1 constarg2 constarg3 =
    conde
      [ a === !!Nil &&& (state =/= !!(St (constarg0, constarg1, constarg2, constarg3)))
      ; Fresh.three (fun x xs newState ->
          a
          === !!(Cons (x, xs))
          &&& (checkStep_true state x
               &&& (step state x newState
                    &&& (checkState_true newState
                         &&& checkAnswerInner_St_false
                               xs
                               newState
                               constarg0
                               constarg1
                               constarg2
                               constarg3))))
      ; Fresh.three (fun x xs newState ->
          a
          === !!(Cons (x, xs))
          &&& (checkStep_true state x
               &&& (step state x newState &&& checkState_false newState)))
      ; Fresh.two (fun x xs -> a === !!(Cons (x, xs)) &&& checkStep_false state x)
      ]
  
  and checkAnswerInner_St_true a state constarg0 constarg1 constarg2 constarg3 =
    a
    === !!Nil
    &&& (state === !!(St (constarg0, constarg1, constarg2, constarg3)))
    ||| Fresh.three (fun x xs newState ->
      a
      === !!(Cons (x, xs))
      &&& (checkStep_true state x
           &&& (step state x newState
                &&& (checkState_true newState
                     &&& checkAnswerInner_St_true
                           xs
                           newState
                           constarg0
                           constarg1
                           constarg2
                           constarg3))))
  
  and checkState_true s =
    Fresh.four (fun i0 g0 c0 w0 -> s === !!(St (i0, g0, c0, w0)) &&& (i0 === g0))
    ||| Fresh.four (fun i0 g0 c0 w0 ->
      s === !!(St (i0, g0, c0, w0)) &&& (i0 =/= g0 &&& (i0 === c0 &&& (i0 === w0))))
  
  and step_St constarg0 constarg1 constarg2 constarg3 p q16 =
    conde
      [ constarg0
        === !!true
        &&& (constarg1
             === !!true
             &&& (p === !!G &&& (q16 === !!(St (!!false, !!false, constarg2, constarg3)))))
      ; constarg0
        === !!true
        &&& (constarg1
             === !!false
             &&& (p === !!G &&& (q16 === !!(St (!!false, !!true, constarg2, constarg3)))))
      ; constarg0
        === !!false
        &&& (constarg1
             === !!true
             &&& (p === !!G &&& (q16 === !!(St (!!true, !!false, constarg2, constarg3)))))
      ; constarg0
        === !!false
        &&& (constarg1
             === !!false
             &&& (p === !!G &&& (q16 === !!(St (!!true, !!true, constarg2, constarg3)))))
      ; constarg0
        === !!true
        &&& (constarg2
             === !!true
             &&& (p === !!C &&& (q16 === !!(St (!!false, constarg1, !!false, constarg3)))))
      ; constarg0
        === !!true
        &&& (constarg2
             === !!false
             &&& (p === !!C &&& (q16 === !!(St (!!false, constarg1, !!true, constarg3)))))
      ; constarg0
        === !!false
        &&& (constarg2
             === !!true
             &&& (p === !!C &&& (q16 === !!(St (!!true, constarg1, !!false, constarg3)))))
      ; constarg0
        === !!false
        &&& (constarg2
             === !!false
             &&& (p === !!C &&& (q16 === !!(St (!!true, constarg1, !!true, constarg3)))))
      ; constarg0
        === !!true
        &&& (constarg3
             === !!true
             &&& (p === !!W &&& (q16 === !!(St (!!false, constarg1, constarg2, !!false)))))
      ; constarg0
        === !!true
        &&& (constarg3
             === !!false
             &&& (p === !!W &&& (q16 === !!(St (!!false, constarg1, constarg2, !!true)))))
      ; constarg0
        === !!false
        &&& (constarg3
             === !!true
             &&& (p === !!W &&& (q16 === !!(St (!!true, constarg1, constarg2, !!false)))))
      ; constarg0
        === !!false
        &&& (constarg3
             === !!false
             &&& (p === !!W &&& (q16 === !!(St (!!true, constarg1, constarg2, !!true)))))
      ; constarg0
        === !!true
        &&& (p === !!N &&& (q16 === !!(St (!!false, constarg1, constarg2, constarg3))))
      ; constarg0
        === !!false
        &&& (p === !!N &&& (q16 === !!(St (!!true, constarg1, constarg2, constarg3))))
      ]
  
  and checkStep_St_true constarg0 constarg1 constarg2 constarg3 step =
    conde
      [ step === !!N
      ; constarg1 === constarg0 &&& (step === !!G)
      ; constarg2 === constarg0 &&& (step === !!C)
      ; constarg3 === constarg0 &&& (step === !!W)
      ]
  
  and checkAnswerInner_St_St_true
    a
    constarg0
    constarg1
    constarg2
    constarg3
    constarg4
    constarg5
    constarg6
    constarg7
    =
    a
    === !!Nil
    &&& (constarg4
         === constarg0
         &&& (constarg5
              === constarg1
              &&& (constarg6 === constarg2 &&& (constarg7 === constarg3))))
    ||| Fresh.three (fun x xs newState ->
      a
      === !!(Cons (x, xs))
      &&& (checkStep_St_true constarg0 constarg1 constarg2 constarg3 x
           &&& (step_St constarg0 constarg1 constarg2 constarg3 x newState
                &&& (checkState_true newState
                     &&& checkAnswerInner_St_true
                           xs
                           newState
                           constarg4
                           constarg5
                           constarg6
                           constarg7))))
  
  and checkAnswerInner_St_St_false
    a
    constarg0
    constarg1
    constarg2
    constarg3
    constarg4
    constarg5
    constarg6
    constarg7
    =
    conde
      [ a
        === !!Nil
        &&& (constarg4
             =/= constarg0
             &&& (constarg5
                  =/= constarg1
                  &&& (constarg6 =/= constarg2 &&& (constarg7 =/= constarg3))))
      ; Fresh.three (fun x xs newState ->
          a
          === !!(Cons (x, xs))
          &&& (checkStep_St_true constarg0 constarg1 constarg2 constarg3 x
               &&& (step_St constarg0 constarg1 constarg2 constarg3 x newState
                    &&& (checkState_true newState
                         &&& checkAnswerInner_St_false
                               xs
                               newState
                               constarg4
                               constarg5
                               constarg6
                               constarg7))))
      ; Fresh.three (fun x xs newState ->
          a
          === !!(Cons (x, xs))
          &&& (checkStep_St_true constarg0 constarg1 constarg2 constarg3 x
               &&& (step_St constarg0 constarg1 constarg2 constarg3 x newState
                    &&& checkState_false newState)))
      ; Fresh.two (fun x xs ->
          a
          === !!(Cons (x, xs))
          &&& checkStep_St_false constarg0 constarg1 constarg2 constarg3 x)
      ]
  
  and checkAnswer_false a =
    checkAnswerInner_St_St_false
      a
      !!true
      !!true
      !!true
      !!true
      !!false
      !!false
      !!false
      !!false
  
  and checkAnswer_true a =
    checkAnswerInner_St_St_true
      a
      !!true
      !!true
      !!true
      !!true
      !!false
      !!false
      !!false
      !!false
  ;;
