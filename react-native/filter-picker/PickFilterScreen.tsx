import React, { useState, useEffect } from 'react';
import { Text } from 'react-native'
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

export default function PickFilterScreen({ route }: Props) {
  const { uri } = route.params;
  return (
    <Text>the uri is: {uri}</Text>
  );
}