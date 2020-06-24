import React, { useState, useEffect } from 'react';
import { Text, ScrollView, View, Button, Image } from 'react-native'
import { RouteProp } from '@react-navigation/native';
import cssgramFilters from 'react-native-image-filter-kit';
import EffectSplit from './EffectSplit';
import { EFFECT_OPTIONS } from './DataEffect'

// type Props = {
//   uri: string
// }

type PickFilterScreenRouteProp = RouteProp<
  RootStackParamList,
  'PickFilter'
>;

type Props = {
  route: PickFilterScreenRouteProp;
}

function getRandomItem<a>(items: Array<a>) {
  return items[Math.floor(Math.random() * items.length)];
}

export default function PickFilterScreen({ route, navigation }: Props) {

  const pickFilter = (name: string) => {
    console.log({ name })
    navigation.navigate('ImageView', { uri: uri, filterName: name })
  }

  const { uri } = route.params;

  const effect1 = getRandomItem(EFFECT_OPTIONS).filter;
  const effect2 = getRandomItem(EFFECT_OPTIONS).filter;

  return (
    <View style={{ flex: 1, padding: 10 }}>
      <EffectSplit
        effect1={effect1}
        effect2={effect2}
        pickFilter={pickFilter}
        uri={uri}
      >
      </EffectSplit>
    </View>

  );
}