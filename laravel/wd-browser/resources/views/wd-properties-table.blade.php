<table>
    <thead>
    <td>Label</td>
    <td>Values</td>
    </thead>
    <tbody>
    @foreach($properties as $property)
        <tr>
            <td>{{$property->label}} (<a href="/entity/{{$property->id}}">{{$property->id}})</a>)</td>
            <td>
                <ul>
                    @foreach ($property->values as $value)
                        <li>
                            @if($value->id[0] === 'Q')
                                <a href="/entity/{{$value->id}}">{{$value->label}}</a>
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
