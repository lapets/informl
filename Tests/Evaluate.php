<?php
include "uxadt.php";
include "Informl.php";
define('_', null);
class Evaluate{\uxadt\_(array('Plus'=>array(_,_),'Minus'=>array(_,_),'Mult'=>array(_,_),'Div'=>array(_,_),'And'=>array(_,_),'Or'=>array(_,_),'Var'=>array(_),'Assign'=>array(_,_,_),'Print'=>array(_,_),'Term'=>array(_),'If'=>array(_,_,_),'True'=>array(),'False'=>array(),'End'=>array(),'Num'=>array(_),'Just'=>array(_),'Nothing'=>array(),'Formula'=>array(_),'Not'=>array(_)));

  
  
  
  function findInEnv($v, $env) {
    foreach ($env as $item){

      if ($item[0] == $v) {
        return $val;
      }
    
}
  }
  
  function evalTerm($term, $env) {
    $__iml0; try {$__iml0 = ($term)->_(Num(_), function ($a) {
    return $a;} )->_(Plus(_,_), function ($a,$b) {
    $x = evalTerm($a, $env);
    $y = evalTerm($b, $env);
    return $informl->plus($x, $y);} )->_(Minus(_,_), function ($a,$b) {
    $x = evalTerm($a, $env);
    $y = evalTerm($b, $env);
    return $informl->plus($x, $y);} )->_(Mult(_,_), function ($a,$b) {
    $x = evalTerm($a, $env);
    $y = evalTerm($b, $env);
    return NULL;} )->_(Div(_,_), function ($a,$b) {
    $x = evalTerm($a, $env);
    $y = evalTerm($b, $env);
    return NULL;} )->_(Var(_), function ($a) {
    return evalTerm(findInEnv($a));} )->end;}catch(Exception $e){return Evaluate_ERROR->PatternMismatch();} if(!isset($__iml0 )) return Evaluate_ERROR->PatternMismatch(); return $__iml0;
  }
  
  function evalForm($form, $env) {
    $__iml1; try {$__iml1 = ($form)->_(True(), function () {
    return $True();} )->_(False(), function () {
    return $False();} )->_(And(_,_), function ($a,$b) {
    $__iml2; try {$__iml2 = (evalForm($a, $env))->_(False(), function () {
    return $False();} )->_(True(), function () {
    return evalForm($b, $env);} )->end;}catch(Exception $e){return Evaluate_ERROR->PatternMismatch();} if(!isset($__iml2 )) return Evaluate_ERROR->PatternMismatch(); return $__iml2;} )->_(Or(_,_), function ($a,$b) {
    $__iml3; try {$__iml3 = (evalForm($a, $env))->_(True(), function () {
    return $True();} )->_(False(), function () {
    return evalForm($b, $env);} )->end;}catch(Exception $e){return Evaluate_ERROR->PatternMismatch();} if(!isset($__iml3 )) return Evaluate_ERROR->PatternMismatch(); return $__iml3;} )->_(Not(_), function ($a) {
    $__iml4; try {$__iml4 = (evalForm($a, $env))->_(True(), function () {
    return $False();} )->_(False(), function () {
    return $True();} )->end;}catch(Exception $e){return Evaluate_ERROR->PatternMismatch();} if(!isset($__iml4 )) return Evaluate_ERROR->PatternMismatch(); return $__iml4;} )->_(Var(_), function ($a) {
    return evalTerm(findInEnv($a));} )->end;}catch(Exception $e){return Evaluate_ERROR->PatternMismatch();} if(!isset($__iml1 )) return Evaluate_ERROR->PatternMismatch(); return $__iml1;
  }
  
  function execute($prog, $env) {
    $__iml5; try {$__iml5 = ($prog)->_(Assign(_,_,_), function ($v,$val,$rest) {
    return execute($rest, $informl->plus(array (array ($v, $val)), $env));} )->_(Print(_,_), function ($o,$rest) {
    $__iml6; try {$__iml6 = ($o)->_(Formula(_), function ($f) {
    return $informl->plus(array (evalForm($f, $env)), execute($rest, $env));} )->_(Term(_), function ($t) {
    return $informl->plus(array (evalTerm($t, $env)), execute($rest, $env));} )->end;}catch(Exception $e){return Evaluate_ERROR->PatternMismatch();} if(!isset($__iml6 )) return Evaluate_ERROR->PatternMismatch(); return $__iml6;} )->_(If(_,_,_), function ($e,$block,$rest) {
    var result; try {result = (evalForm($e, $env))->_(True(), function () {
    return execute($block, $env);} )->_(False(), function () {
    return array ();} ).end;}catch(Exception $e){return Evaluate_ERROR->PatternMismatch();} if(!isset($result )) return Evaluate_ERROR->PatternMismatch();
    return $informl->plus($result, execute($rest, $env));} )->_(End(), function () {
    return array ();} )->end;}catch(Exception $e){return Evaluate_ERROR->PatternMismatch();} if(!isset($__iml5 )) return Evaluate_ERROR->PatternMismatch(); return $__iml5;
  }
  
  function maybeNumTimes2($n) {
    $__iml7; try {$__iml7 = ($n)->_(Num(_), function ($a) {
    return $Just(NULL);} )->end;}catch(err){
    return $Nothing();}if(!isset($__iml7 )) {
    return $Nothing();} return $__iml7;
  }
  
  function maybeNot($f) {
    var form; try {form = ($f)->_(Formula(_), function ($a) {
    return $a;} ).end;}catch(Exception $e){return Evaluate_ERROR->PatternMismatch();} if(!isset($form )) return Evaluate_ERROR->PatternMismatch();
    return evalForm($Not($form));
  }
} ?>