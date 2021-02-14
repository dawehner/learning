<table>
    <thead>
    <td>Label</td>
    <td>Values</td>
    </thead>
    <tbody>
    @foreach($properties as $property)
        <tr>
            <td>{{$property->label}} (@include('wd-link', ['title' => $property->id, 'ref' => $property->id]))</td>
            <td>
                <ul>
                    @foreach ($property->values as $value)
                        <li>
                            @if($value->id[0] === 'Q')
                                @include('wd-link', ['title' => $value->label, 'ref' => $value->id])
                            @else
                                {{ $value->label }}

                                @if (count($value->qualifiers))
                                    <ul>
                                        @foreach ($value->qualifiers as $qualifier)
                                            <li>{{$qualifier->label}}: {{$qualifier->value}}</li>
                                        @endforeach
                                    </ul>
                                @endif
                            @endif
                        </li>
                    @endforeach
                </ul>
            </td>
        </tr>
    @endforeach
    </tbody>
</table>
