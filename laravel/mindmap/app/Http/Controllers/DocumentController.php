<?php

namespace App\Http\Controllers;

use App\Document;
use Illuminate\Http\Response;

class DocumentController extends Controller
{
    /**
     * @return Response
     */
    public function list() {
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
    
}
