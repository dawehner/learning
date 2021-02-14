<?php

namespace App\Http\Middleware;

use Closure;
use Illuminate\Http\Request;

class BrowserHistory
{

    /**
     * Handle an incoming request.
     *
     * @param \Illuminate\Http\Request $request
     * @param \Closure $next
     *
     * @return mixed
     */
    public function handle(Request $request, Closure $next)
    {
        $request->session()->push('history', $request->getPathInfo());
        return $next($request);
    }

}
