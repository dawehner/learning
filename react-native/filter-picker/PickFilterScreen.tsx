import React, { useState, useEffect } from 'react';
import { Text, ScrollView, View, Button } from 'react-native'
import { RouteProp } from '@react-navigation/native';

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

export default function PickFilterScreen({ route, navigation }: Props) {

  const pickFilter = (name: string) => () => {
    navigation.navigate('ImageView', { uri: uri, filterName: name })
  }

  const { uri } = route.params;
  return (
    <View>
      <Text>the uri is: {uri}</Text>
      <ScrollView
        horizontal={true}
      >
        <Button title="Filter 1" onPress={pickFilter("filter_1")}></Button>
        <Button title="Filter 2" onPress={pickFilter("filter_1")}></Button>
      </ScrollView>
    </View>

  );
}