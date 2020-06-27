import React, { ReactNode } from 'react'

// Data Effect
import { EFFECT_OPTIONS, LIST_COMPONENTS } from './DataEffect'
import { ImageFilterProps, ConfigWithIntermediates } from 'react-native-image-filter-kit';

type Props = {
  effect: string;
  children: ReactNode;
}

export default function Effect({ children, effect, ...props }: Props) {
  const effectOptions = EFFECT_OPTIONS.filter(effectOption => effectOption.filter === effect);
  const effectOption = effectOptions.shift();

  if (!effectOption) {
    return children;
  }

  const EffectComponent: React.Component<ImageFilterProps<ConfigWithIntermediates>> = LIST_COMPONENTS[effectOption.id];

  return EffectComponent ? (<EffectComponent {...props} image={children} />) : null;
}