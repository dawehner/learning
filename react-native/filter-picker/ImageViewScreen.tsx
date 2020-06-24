import React, { useState, useEffect } from 'react';
import { Text, ScrollView, View, Button, Image } from 'react-native'
import { RouteProp } from '@react-navigation/native';
import { ToasterCompat } from 'react-native-image-filter-kit'
import Share from 'react-native-share';
import Effect from './Effect';

// import { CssGram, ImageFilter } from 'react-native-image-filter-kit';

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

// {/* image=
//           <Image
//           style={{ width: 320, height: 320 }}
//           source={{
//             uri: <CssGram>

//             </CssGram>
//           }}
//           resizeMode={'contain'}
//         /> */}

export default function ImageViewScreen({ route }: Props) {


  const { uri, filterName } = route.params;

  const imageStyle = { width: 320, height: 320 }
  const image = (<Image source={{ uri: uri }} style={imageStyle} resizeMode={'contain'} />);

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
    <View>
      <Effect effect={filterName}>
        {image}
      </Effect>
      <Button onPress={shareImage} title="Share image" />
    </View>

  );
}