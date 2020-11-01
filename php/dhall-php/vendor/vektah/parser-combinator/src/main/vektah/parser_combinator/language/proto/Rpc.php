<?php

namespace vektah\parser_combinator\language\proto;

class Rpc
{
    public $method;
    public $request;
    public $response;

    public function __construct($method, $request, $response)
    {
        $this->response = $response;
        $this->method = $method;
        $this->request = $request;
    }
}
