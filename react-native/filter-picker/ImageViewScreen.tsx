import React, { useState, useEffect } from 'react';
import { Text, ScrollView, View, Button, Image } from 'react-native'
import { RouteProp } from '@react-navigation/native';

// type Props = {
//   uri: string
// }

type ImageViewScreenRouteProps = RouteProp<
  RootStackParamList,
  'ImageView'
>;

type Props = {
  route: ImageViewScreenRouteProps;
}

export default function ImageViewScreen({ route }: Props) {


  const { uri, filterName } = route.params;
  return (
    <View>
      <Text>the uri is: {uri}</Text>
      <Text>the filter is: {filterName}</Text>
      <View style={{
        flex: 1,
        flexDirection: 'row',
        justifyContent: 'center',
      }}>
        <Image
          style={{ width: 320, height: 320 }}
          source={{ uri }}
          resizeMode={'contain'}
        />
      </View>
    </View>

  );
}