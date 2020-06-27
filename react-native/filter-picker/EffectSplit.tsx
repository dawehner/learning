import React from 'react'
import { View, Text, TouchableHighlight, Image } from 'react-native'
import Effect from './Effect'

type Props = {
  effect1: string;
  effect2: string;
  uri: string;
  pickFilter: Function,
}

export default function EffectSplit({ effect1, effect2, uri, pickFilter }: Props) {
  const imageStyle = {
    height: 320,
    width: 320
  }
  const image = (<Image source={{ uri: uri }} style={imageStyle} resizeMode={'contain'} />);
  return (
    <View style={{
      flex: 6,
      flexDirection: 'column',
      // justifyContent: "space-around",
      height: '100%',
    }}>
      <View style={{ flex: 3.5, justifyContent: 'center' }}>
        <Text style={{ flex: 0.5 }}>{effect1}</Text>
        <TouchableHighlight
          style={{ flex: 3 }}
          activeOpacity={0.6}
          underlayColor="#DDDDDD"
          onPress={() => pickFilter(effect1)}
        >
          <Effect effect={effect1}>{image}</Effect>
        </TouchableHighlight>
      </View>


      <View style={{ flex: 3.5, justifyContent: 'flex-end' }}>
        <Text style={{ flex: 0.5 }}>{effect2}</Text>
        <TouchableHighlight
          style={{ flex: 3 }}
          activeOpacity={0.6}
          underlayColor="#DDDDDD"
          onPress={() => pickFilter(effect2)}
        >
          <Effect effect={effect2}>{image}</Effect>
        </TouchableHighlight>
      </View>
    </View>
  )
}