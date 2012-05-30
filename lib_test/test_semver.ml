open OUnit
open Semver

let test_parse_print = fun ver str () -> begin
  assert_equal ver (parse_version str);
  assert_equal (print_version ver) str
end

let eq = fun v1 v2 -> assert_equal (compare_version v1 v2)  0
let gt = fun v1 v2 -> begin
  assert_equal (compare_version v1 v2)  1;
  assert_equal (compare_version v2 v1) (-1);
end 

let v = parse_version

let q input versions expected = assert_equal (query input (List.map v versions)) expected

let suite = "Semver suite">::: [
  "3 parts">:: test_parse_print (Semver (1, 1, 1)) "1.1.1";
  "long parts">:: test_parse_print (Semver (1243, 32415, 54535)) "1243.32415.54535";

  "inc">:: begin fun () -> 
    eq (v"0.0.1") (increment_version `Patch (v"0.0.0"));
    eq (v"0.1.0") (increment_version `Minor (v"0.0.0"));
    eq (v"1.0.0") (increment_version `Major (v"0.0.0"));
    
    eq (v"0.1.3") (increment_version `Patch (v"0.1.2"));
    eq (v"1.1.0") (increment_version `Minor (v"1.0.2"));
    eq (v"1.0.0") (increment_version `Major (v"0.1.2"));
  end;

  "cmp">:: begin fun () -> 
    eq (v"1.1.1") (v"1.1.1");

    gt (v"1.1.1") (v"1.1.0");
    gt (v"1.1.1") (v"1.0.1");
    gt (v"1.1.1") (v"0.1.1");
  end;

  "query">:: begin fun () ->
    q "1.1.1" ["1.1.1"] (Some (v"1.1.1"));
    q "1.1" ["1.1.1"] (Some (v"1.1.1"));
    q "1" ["1.1.1"] (Some (v"1.1.1"));
    q "2" ["1.1.1"] None;

    q "1.1.1" ["2.1.1"; "1.1.1"; "1.1.0"; "1.1.2"; "1.2.1"] (Some (v"1.1.1"));
    q "1.1" ["2.1.1"; "1.1.1"; "1.1.0"; "1.1.2"; "1.2.1"] (Some (v"1.1.2"));
    q "1" ["2.1.1"; "1.1.1"; "1.1.0"; "1.1.2"; "1.2.1"] (Some (v"1.2.1"));
    q "2.1" ["2.1.1"; "1.1.1"; "1.1.0"; "1.1.2"; "1.2.1"] (Some (v"2.1.1"));
    q "0.5" ["2.1.1"; "1.1.1"; "1.1.0"; "1.1.2"; "1.2.1"] None;
  end;
]
