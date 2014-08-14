<?php

namespace Informl;

    function size($o) {
        if (is_string($o)) return strlen($o);
        if (is_array($o)) return count($o);
    }
    
    function plus($o1,$o2) {
        if (is_null($o1) || is_null($o2)) return NULL;
        if (is_array($o1) && is_array($o2)) return array_merge($o1,$o2);
        if (is_array($o1)) return array_merge($o1, array($o2));
        if (is_array($o2)) return array_merge(array($o1), $o2);
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
        return array_map($f,$o);
    }

    function fold($o,$f,$b) {
        $acc = $b;
        for($i = count($o); $i > 0; $i--) {
            $acc = call_user_func($f,array($o[$i-1],$acc));
        }

        return $acc;
    }


?>