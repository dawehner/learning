<?php

namespace App\Http\Controllers\API;

use App\Document;
use Illuminate\Support\Facades\Validator;
use Illuminate\Http\Request;
use App\Http\Controllers\Controller;

class DocumentController extends Controller
{
    /**
     * Display a listing of the resource.
     *
     * @return \Illuminate\Http\Response
     */
    public function index()
    {
        $documents = Document::all();

        $data = array_map(function (Document $document) {
            return [
                'id' => $document->id,
                'title' => $document->title,
            ];
        }, iterator_to_array(
            $documents
        ));

        return response()->json($data);
    }

    /**
     * Store a newly created resource in storage.
     *
     * @param  \Illuminate\Http\Request $request
     * @return \Illuminate\Http\JsonResponse
     */
    public function store(Request $request)
    {
        $validator = Validator::make($request->all(), [
            'title' => 'string|required',
        ]);

        if ($validator->fails()) {
            return response('', 422);
        }
        $document = Document::create([
            'title' => $request->input('title'),
        ]);
        $document
            ->save();

        return \response()->json([
            'id' => $document->id,
            'title' => $document->title,
        ])->setStatusCode(201);
    }

    /**
     * Display the specified resource.
     *
     * @param Document $document
     * @return \Illuminate\Http\JsonResponse
     */
    public function show(Document $document)
    {
        return \response()->json([
            'id' => $document->id,
            'title' => $document->title,
        ]);
    }

    /**
     * Update the specified resource in storage.
     *
     * @param  \Illuminate\Http\Request $request
     * @param  int $id
     * @return \Illuminate\Http\Response
     */
    public function update(Request $request, $id)
    {
        //
    }

    /**
     * Remove the specified resource from storage.
     *
     * @param Document $document
     * @return \Illuminate\Http\Response
     *
     * @throws \Exception
     */
    public function destroy(Document $document)
    {
        $document->delete();

        return \response('', 204);
    }
}
