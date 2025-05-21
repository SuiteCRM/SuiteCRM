<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\CloudNaturalLanguage;

class XPSVisualization extends \Google\Model
{
  /**
   * @var float
   */
  public $clipPercentLowerbound;
  /**
   * @var float
   */
  public $clipPercentUpperbound;
  /**
   * @var string
   */
  public $colorMap;
  /**
   * @var string
   */
  public $overlayType;
  /**
   * @var string
   */
  public $polarity;
  /**
   * @var string
   */
  public $type;

  /**
   * @param float
   */
  public function setClipPercentLowerbound($clipPercentLowerbound)
  {
    $this->clipPercentLowerbound = $clipPercentLowerbound;
  }
  /**
   * @return float
   */
  public function getClipPercentLowerbound()
  {
    return $this->clipPercentLowerbound;
  }
  /**
   * @param float
   */
  public function setClipPercentUpperbound($clipPercentUpperbound)
  {
    $this->clipPercentUpperbound = $clipPercentUpperbound;
  }
  /**
   * @return float
   */
  public function getClipPercentUpperbound()
  {
    return $this->clipPercentUpperbound;
  }
  /**
   * @param string
   */
  public function setColorMap($colorMap)
  {
    $this->colorMap = $colorMap;
  }
  /**
   * @return string
   */
  public function getColorMap()
  {
    return $this->colorMap;
  }
  /**
   * @param string
   */
  public function setOverlayType($overlayType)
  {
    $this->overlayType = $overlayType;
  }
  /**
   * @return string
   */
  public function getOverlayType()
  {
    return $this->overlayType;
  }
  /**
   * @param string
   */
  public function setPolarity($polarity)
  {
    $this->polarity = $polarity;
  }
  /**
   * @return string
   */
  public function getPolarity()
  {
    return $this->polarity;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSVisualization::class, 'Google_Service_CloudNaturalLanguage_XPSVisualization');
