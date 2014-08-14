<?php namespace Evaluate;
include "uxadt.php";
include "Informl.php";

\uxadt\_('Test', array('Num'=>array(NULL),'Plus'=>array(NULL,NULL),'Minus'=>array(NULL,NULL),'Mult'=>array(NULL,NULL),'Div'=>array(NULL,NULL),'And'=>array(NULL,NULL),'Or'=>array(NULL,NULL),'Not'=>array(NULL),'Assign'=>array(NULL,NULL),'Var'=>array(NULL),'Print'=>array(NULL),'Term'=>array(NULL),'If'=>array(NULL,NULL),'Formula'=>array(NULL),'Vero'=>array(),'Falso'=>array(),'Program'=>array(NULL),'This'=>array(NULL),'That'=>array(NULL,NULL),'Alp'=>array()));


  
  
  
  function evalTerm($term, $env) {
    $__iml0 = ($term);$__iml1 = $__iml0->_(Num(NULL), function(){return 1;})->_(Plus(NULL,NULL), function(){return 1;})->_(Minus(NULL,NULL), function(){return 1;})->_(Mult(NULL,NULL), function(){return 1;})->_(Div(NULL,NULL), function(){return 1;})->_(Var(NULL), function(){return 1;})->end;
    if ($__iml1 != 1){
    return "wrong";}
    $__iml_is2 = $__iml0;
    if( ($__iml0)->(Num(NULL))){
    list($a) = $__iml_is2;
      return $a;
    }$__iml_is3 = $__iml0;
    if( ($__iml0)->(Plus(NULL,NULL))){
    list($a,$b) = $__iml_is3;
      $x = evalTerm($a, $env);
      $y = evalTerm($b, $env);
      return \Informl\plus($x, $y);
    }$__iml_is4 = $__iml0;
    if( ($__iml0)->(Minus(NULL,NULL))){
    list($a,$b) = $__iml_is4;
      $x = evalTerm($a, $env);
      $y = evalTerm($b, $env);
      return \Informl\minus($x, $y);
    }$__iml_is5 = $__iml0;
    if( ($__iml0)->(Mult(NULL,NULL))){
    list($a,$b) = $__iml_is5;
      $x = evalTerm($a, $env);
      $y = evalTerm($b, $env);
      return ($x * $y);
    }$__iml_is6 = $__iml0;
    if( ($__iml0)->(Div(NULL,NULL))){
    list($a,$b) = $__iml_is6;
      $x = evalTerm($a, $env);
      $y = evalTerm($b, $env);
      return ($x / $y);
    }$__iml_is7 = $__iml0;
    if( ($__iml0)->(Var(NULL))){
    list($a) = $__iml_is7;
      $__iml8 = ($env[$a]);$__iml9 = $__iml8->_(Term(NULL), function(){return 1;})->end;
      if ($__iml9 != 1){throw new \Exception('Pattern Mismatch');}
      $__iml_is10 = $__iml8;
      if( ($__iml8)->(Term(NULL))){
      list($t) = $__iml_is10;
        return evalTerm($t, $env);
      }
    }
  }
  
  function evalForm($form, $env) {
    $__iml11 = ($form);$__iml12 = $__iml11->_(Vero(), function(){return 1;})->_(Falso(), function(){return 1;})->_(And(NULL,NULL), function(){return 1;})->_(Or(NULL,NULL), function(){return 1;})->_(Not(NULL), function(){return 1;})->_(Var(NULL), function(){return 1;})->end;
    if ($__iml12 != 1){throw new \Exception('Pattern Mismatch');}
    $__iml_is13 = $__iml11;
    if( ($__iml11)->(Vero())){
    list() = $__iml_is13;
      return Vero();
    }$__iml_is14 = $__iml11;
    if( ($__iml11)->(Falso())){
    list() = $__iml_is14;
      return Falso();
    }$__iml_is15 = $__iml11;
    if( ($__iml11)->(And(NULL,NULL))){
    list($a,$b) = $__iml_is15;
      $__iml16 = (evalForm($a, $env));$__iml17 = $__iml16->_(Falso(), function(){return 1;})->_(Vero(), function(){return 1;})->end;
      if ($__iml17 != 1){throw new \Exception('Pattern Mismatch');}
      $__iml_is18 = $__iml16;
      if( ($__iml16)->(Falso())){
      list() = $__iml_is18;
        return Falso();
      }$__iml_is19 = $__iml16;
      if( ($__iml16)->(Vero())){
      list() = $__iml_is19;
        return evalForm($b, $env);
      }
    }$__iml_is20 = $__iml11;
    if( ($__iml11)->(Or(NULL,NULL))){
    list($a,$b) = $__iml_is20;
      $__iml21 = (evalForm($a, $env));$__iml22 = $__iml21->_(Vero(), function(){return 1;})->_(Falso(), function(){return 1;})->end;
      if ($__iml22 != 1){throw new \Exception('Pattern Mismatch');}
      $__iml_is23 = $__iml21;
      if( ($__iml21)->(Vero())){
      list() = $__iml_is23;
        return Vero();
      }$__iml_is24 = $__iml21;
      if( ($__iml21)->(Falso())){
      list() = $__iml_is24;
        return evalForm($b, $env);
      }
    }$__iml_is25 = $__iml11;
    if( ($__iml11)->(Not(NULL))){
    list($a) = $__iml_is25;
      $__iml26 = (evalForm($a, $env));$__iml27 = $__iml26->_(Vero(), function(){return 1;})->_(Falso(), function(){return 1;})->end;
      if ($__iml27 != 1){throw new \Exception('Pattern Mismatch');}
      $__iml_is28 = $__iml26;
      if( ($__iml26)->(Vero())){
      list() = $__iml_is28;
        return Falso();
      }$__iml_is29 = $__iml26;
      if( ($__iml26)->(Falso())){
      list() = $__iml_is29;
        return Vero();
      }
    }$__iml_is30 = $__iml11;
    if( ($__iml11)->(Var(NULL))){
    list($a) = $__iml_is30;
      $__iml31 = ($env[$a]);$__iml32 = $__iml31->_(Formula(NULL), function(){return 1;})->end;
      if ($__iml32 != 1){throw new \Exception('Pattern Mismatch');}
      $__iml_is33 = $__iml31;
      if( ($__iml31)->(Formula(NULL))){
      list($f) = $__iml_is33;
        return evalForm($f, $env);
      }
    }
  }
  
  function evalStatement($stmt, $env) {
    $__iml34 = ($stmt);$__iml35 = $__iml34->_(Assign(NULL,NULL), function(){return 1;})->_(Print(NULL), function(){return 1;})->_(If(NULL,NULL), function(){return 1;})->end;
    if ($__iml35 != 1){throw new \Exception('Pattern Mismatch');}
    $__iml_is36 = $__iml34;
    if( ($__iml34)->(Assign(NULL,NULL))){
    list($v,$val) = $__iml_is36;
      $__iml37 = ($v);$__iml38 = $__iml37->_(Var(NULL), function(){return 1;})->end;
      if ($__iml38 != 1){throw new \Exception('Pattern Mismatch');}
      $__iml_is39 = $__iml37;
      if( ($__iml37)->(Var(NULL))){
      list($a) = $__iml_is39;
        $env[$a] = $val;
        return NULL;
      }
    }$__iml_is40 = $__iml34;
    if( ($__iml34)->(Print(NULL))){
    list($o) = $__iml_is40;
      $__iml41 = ($o);$__iml42 = $__iml41->_(Formula(NULL), function(){return 1;})->_(Term(NULL), function(){return 1;})->end;
      if ($__iml42 != 1){throw new \Exception('Pattern Mismatch');}
      $__iml_is43 = $__iml41;
      if( ($__iml41)->(Formula(NULL))){
      list($f) = $__iml_is43;
        return evalForm($f, $env);
      }$__iml_is44 = $__iml41;
      if( ($__iml41)->(Term(NULL))){
      list($t) = $__iml_is44;
        return evalTerm($t, $env);
      }
    }$__iml_is45 = $__iml34;
    if( ($__iml34)->(If(NULL,NULL))){
    list($e,$block) = $__iml_is45;
      $__iml46 = (evalForm($e, $env));$__iml47 = $__iml46->_(Vero(), function(){return 1;})->_(Falso(), function(){return 1;})->end;
      if ($__iml47 != 1){throw new \Exception('Pattern Mismatch');}
      $__iml_is48 = $__iml46;
      if( ($__iml46)->(Vero())){
      list() = $__iml_is48;
        return evalStatement($block, $env);
      }$__iml_is49 = $__iml46;
      if( ($__iml46)->(Falso())){
      list() = $__iml_is49;
        return NULL;
      }
    }
  }
  
  function execute($prog) {
    $env = array ();
    $__iml50 = ($prog);$__iml51 = $__iml50->_(Program(NULL), function(){return 1;})->end;
    if ($__iml51 != 1){throw new \Exception('Pattern Mismatch');}
    $__iml_is52 = $__iml50;
    if( ($__iml50)->(Program(NULL))){
    list($ss) = $__iml_is52;
      $out = \Informl\map($ss, function($x){return evalStatement($x, $env);});
      $ret = array ();
      foreach ($out as $o){
      
        if ($o != NULL) {
          $ret = \Informl\plus($ret, $o);
        }
      }
      
      return $ret;
    }
  }
  
  function getTest($x) {
    $__iml53 = ($x);$__iml54 = $__iml53->_(This(NULL), function(){return 1;})->end;
    if ($__iml54 != 1){
    return "otherwise get";}
    $__iml_is55 = $__iml53;
    if( ($__iml53)->(This(NULL))){
    list($a) = $__iml_is55;
      return "hi";
    }
  }
  
  function setTest($x) {
    $__iml56 = ($x); $tmp = $__iml56->_(That(NULL,NULL), function(){return 1;})->end;
    if ($tmp != 1){$tmp = "otherwise set";}else $tmp = $__iml56->_(That(NULL,NULL), function ($a) {
    return "hi";} )->end;
    return $tmp;
  }
 ?>