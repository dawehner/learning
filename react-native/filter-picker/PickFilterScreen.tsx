import React, { useState } from 'react';
import { View, Button, Text } from 'react-native'
import { RouteProp } from '@react-navigation/native';
import AsyncStorage from '@react-native-community/async-storage';
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

const storeChosenFilter = async (name: string) => {
  try {
    const key = '@fp/chosen-filters';
    let chosenFiltersJson = await AsyncStorage.getItem(key);
    let chosenFilters = (chosenFiltersJson ? JSON.parse(chosenFiltersJson) : []) || [];
    chosenFilters.push(name);

    await AsyncStorage.setItem(key, JSON.stringify(chosenFilters))
  } catch (e) {
    console.error({ e });
  }
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

  const favourFilter = (name: string) => {
    const filterToRemove = name === effect1 ? effect2 : effect1;
    const nextRemainingFilters = remainingFilters.filter(a => a.filter !== filterToRemove);

    if (nextRemainingFilters.length === 0) {
      storeChosenFilter(name);
      navigation.navigate('ImageView', { fullUri: route.params.fullUri, uri: route.params.uri, filterName: name })
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
    <View style={{ flex: 8, padding: 10 }}>
      <Text style={{ flex: 0.5 }}>{remainingFilters.length} | {EFFECT_OPTIONS.length}</Text>
      <Button style={{ flex: 0.5 }}
        title="Shuffle"
        onPress={() => shuffle()}
      />
      <EffectSplit
        effect1={effect1}
        effect2={effect2}
        pickFilter={e => favourFilter(e)}
        uri={uri}
      >
      </EffectSplit>
    </View>
  );
}
