import React from 'react';
import { View, Button, Image } from 'react-native'
import { RouteProp } from '@react-navigation/native';
import Share from 'react-native-share';
import Effect from './Effect';


type ImageViewScreenRouteProps = RouteProp<
  RootStackParamList,
  'ImageView'
>;

type Props = {
  route: ImageViewScreenRouteProps;
}

export default function ImageViewScreen({ route }: Props) {
  const { uri, filterName } = route.params;

  const shareOptions = {
    title: 'Share image',
    url: uri,
  };
  const shareImage = () => {
    Share.open(shareOptions)
      .then((res) => { console.log(res) })
      .catch((err) => { err && console.log(err); });
  };

  return (
    <View style={{ flex: 1, padding: 10, justifyContent: 'center', alignItems: 'center' }}>
      <Effect effect={filterName} style={{ flex: 1 }}>
        <Image
          source={{ uri: uri }}
          style={{ height: 320, width: 320, resizeMode: 'contain' }}
          resizeMode="contain"
          resizeMethod="resize"
        />
      </Effect>
      <Button onPress={shareImage} title="Share image" />
    </View>
  );
}