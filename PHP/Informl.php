<?php

namespace Informl;

    /**
    * dictionary wrapper class
    */
    class Dictionary 
    {
        public $arr = array();
        public function __construct(/*...*/)
        {
            $args = func_get_args();
            for( $i=0, $n=count($args); $i<$n; $i++ )
                $this->add($args[$i]);
        }

        public function add($keyval) {
            if(is_array($keyval)) {
               list($key,$val) = $keyval;
               $this->arr[$key] = $val; 
            }
        }

        public function get( /*string*/ $name = null ) {
            return $this->arr[$name];
        }
    }

    class ListIml
    {
        public $arr = array();
        public function __construct(/*...*/)
        {
            $args = func_get_args();
            for( $i=0, $n=count($args); $i<$n; $i++ )
                $this->add($args[$i]);
        }
        public function add($val) {
            array_push($this->arr, $val);
        }

    }

    function size($o) {
        if (is_string($o)) return strlen($o);
        if (is_array($o)) return count($o);
    }
    
    function plus($o1,$o2) {
        if (is_null($o1) || is_null($o2)) return NULL;
        if (($o1 instanceof ListIml) && ($o2 instanceof ListIml)){
            $ar = array_merge($o1->arr,$o2->arr);
            $lst = new ListIml();
            for($i = 0; $i < count($ar); $i++)
                $lst->add($ar[$i]);
            return $lst;
        } 
        if ($o1 instanceof ListIml) {
            $ar = $o1->arr;
            $lst = new ListIml();
            for($i = 0; $i < count($ar); $i++)
                $lst->add($ar[$i]);
            $lst->add($o2);
            return $lst;
        }
        if ($o2 instanceof ListIml) {
            $ar = $o2->arr;
            $lst = new ListIml();
            $lst->add($o1);
            for($i = 0; $i < count($ar); $i++)
                $lst->add($ar[$i]);
            return $lst;
        }
        if (is_string($o1) && is_string($o2)) return $o1.$o2;
        if (is_string($o1)) return $o1.strval($o2);
        if (is_string($o2)) return strval($o1).$o2;
        return $o1 + $o2;
    }

    function minus($o1,$o2) {
        if (is_null($o1) || is_null($o2)) return NULL;
        return $o1 - $o2;
    }

    function map($o,$f) {
        $ar = array_map($f,$o->arr);
        $lst = new ListIml();
        $lst->arr = $ar;
        return $lst;
    }

    function fold($o,$f,$b) {
        $acc = $b;
        for($i = count($o); $i > 0; $i--) {
            $acc = call_user_func($f,array($o->arr[$i-1],$acc));
        }

        return $acc;
    }

    function range($start,$end){
        $lst = new ListIml();
        $lst->arr = range($start,$end - 1);
        return $lst;
    }

    function type($o){
        if(is_bool($o)) return "boolean";
        if(is_numeric($o)) return "number";
        if(is_string($o)) return "string";
        if(is_callable($o)) return "function";
        if($o instanceof ListIml) return "list";
        if($o instanceof Dictionary) return "dictionary";
        if($o instanceof \uxadt\Value) return "pattern";
        return "null";
    }

    function slice($o,$start,$end){
        if($o instanceof ListIml) {
            $output;
            $ar = $o->arr;
            if(is_null($end)){
                $output = array_slice($ar,$start);
            }
            else if(is_null($start)) {
                if($end < 0) $end = $end + count($ar);
                $output = array_slice($ar, 0, $end);
            }
            else {
                if($end < 0) $end = $end + count($ar);
                $output = array_slice($ar, $start, $end - $start);
            }
            $lst = new ListIml();
            $lst->arr = $output;
            return $lst;
        }
        else if(is_string($o)) {
            if(is_null($end)){
                return substr($o,$start);
            }
            else if(is_null($start)) {
                if($end < 0) $end = $end + count($o);
                return substr($o, 0, $end);
            }
            else {
                if($end < 0) $end = $end + count($o);
                return substr($o, $start, $end - $start);
            }
        }
        else return NULL;

    }

    function inarray($o, $ar){
        if (type($o) != "dictionary" && type($o) != "list") 
            return FALSE;
        return in_array($o, $ar->arr);
    }


?>