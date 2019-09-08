<?php

use Illuminate\Http\Request;

/*
|--------------------------------------------------------------------------
| API Routes
|--------------------------------------------------------------------------
|
| Here is where you can register API routes for your application. These
| routes are loaded by the RouteServiceProvider within a group which
| is assigned the "api" middleware group. Enjoy building your API!
|
*/

Route::middleware('auth:api')->get('/user', function (Request $request) {
    return $request->user();
});

Route::get('/document', 'API\DocumentController@index');
Route::get('/document/{document}', 'API\DocumentController@show');
Route::get('/document/{document}/full', 'API\DocumentController@showFull');
Route::delete('/document/{document}', 'API\DocumentController@destroy');
Route::post('/document', 'API\DocumentController@store');

