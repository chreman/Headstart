<?php

namespace headstart\utils;

class ExecutionTime
{
     private $startTime;
     private $endTime;


     public function start(){
         $this->startTime = microtime(true);
     }
     public function end(){
         $this->endTime =  microtime(true);
     }
     public function diff(){
         return $this->endTime - $this->startTime;
     }
}
