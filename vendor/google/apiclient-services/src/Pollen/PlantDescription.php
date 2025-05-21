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

namespace Google\Service\Pollen;

class PlantDescription extends \Google\Model
{
  /**
   * @var string
   */
  public $crossReaction;
  /**
   * @var string
   */
  public $family;
  /**
   * @var string
   */
  public $picture;
  /**
   * @var string
   */
  public $pictureCloseup;
  /**
   * @var string
   */
  public $season;
  /**
   * @var string
   */
  public $specialColors;
  /**
   * @var string
   */
  public $specialShapes;
  /**
   * @var string
   */
  public $type;

  /**
   * @param string
   */
  public function setCrossReaction($crossReaction)
  {
    $this->crossReaction = $crossReaction;
  }
  /**
   * @return string
   */
  public function getCrossReaction()
  {
    return $this->crossReaction;
  }
  /**
   * @param string
   */
  public function setFamily($family)
  {
    $this->family = $family;
  }
  /**
   * @return string
   */
  public function getFamily()
  {
    return $this->family;
  }
  /**
   * @param string
   */
  public function setPicture($picture)
  {
    $this->picture = $picture;
  }
  /**
   * @return string
   */
  public function getPicture()
  {
    return $this->picture;
  }
  /**
   * @param string
   */
  public function setPictureCloseup($pictureCloseup)
  {
    $this->pictureCloseup = $pictureCloseup;
  }
  /**
   * @return string
   */
  public function getPictureCloseup()
  {
    return $this->pictureCloseup;
  }
  /**
   * @param string
   */
  public function setSeason($season)
  {
    $this->season = $season;
  }
  /**
   * @return string
   */
  public function getSeason()
  {
    return $this->season;
  }
  /**
   * @param string
   */
  public function setSpecialColors($specialColors)
  {
    $this->specialColors = $specialColors;
  }
  /**
   * @return string
   */
  public function getSpecialColors()
  {
    return $this->specialColors;
  }
  /**
   * @param string
   */
  public function setSpecialShapes($specialShapes)
  {
    $this->specialShapes = $specialShapes;
  }
  /**
   * @return string
   */
  public function getSpecialShapes()
  {
    return $this->specialShapes;
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
class_alias(PlantDescription::class, 'Google_Service_Pollen_PlantDescription');
