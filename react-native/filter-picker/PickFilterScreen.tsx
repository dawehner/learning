import React, { useState } from 'react';
import { View, Button, Text } from 'react-native'
import { RouteProp } from '@react-navigation/native';
import EffectSplit from './EffectSplit';
import { EFFECT_OPTIONS } from './DataEffect'

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

function shuffleArray<T>(array: Array<T>): Array<T> {
  for (var i = array.length - 1; i > 0; i--) {
    var j = Math.floor(Math.random() * (i + 1));
    var temp = array[i];
    array[i] = array[j];
    array[j] = temp;
  }

  return array
}

export default function PickFilterScreen({ route, navigation }: Props) {
  const [remainingFilters, setRemainingFilters] = useState(shuffleArray(EFFECT_OPTIONS))
  const [effect1, setEffect1] = useState(getRandomItem(EFFECT_OPTIONS).filter)
  const [effect2, setEffect2] = useState(getRandomItem(EFFECT_OPTIONS).filter)

  const pickFilter = (name: string) => {
    const nextRemainingFilters = remainingFilters.filter(a => a.filter !== name);

    if (nextRemainingFilters.length === 0) {
      navigation.navigate('ImageView', { uri: route.params.fullUri, filterName: name })
    }
    else {
      setRemainingFilters(nextRemainingFilters);
      if (name === effect1) {
        setEffect2(getRandomItem(nextRemainingFilters).filter);
      }
      else {
        setEffect1(getRandomItem(nextRemainingFilters).filter);
      }
    }
  }

  const shuffle = () => {
    setEffect1(getRandomItem(EFFECT_OPTIONS).filter);
    setEffect2(getRandomItem(EFFECT_OPTIONS).filter);
  }

  const { uri } = route.params;

  return (
    <View style={{ flex: 1, padding: 10 }}>
      <Text>{remainingFilters.length} | {EFFECT_OPTIONS.length}</Text>
      <Button
        title="Shuffle"
        onPress={() => shuffle()}
      />
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
