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

class PlantInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $code;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var bool
   */
  public $inSeason;
  protected $indexInfoType = IndexInfo::class;
  protected $indexInfoDataType = '';
  protected $plantDescriptionType = PlantDescription::class;
  protected $plantDescriptionDataType = '';

  /**
   * @param string
   */
  public function setCode($code)
  {
    $this->code = $code;
  }
  /**
   * @return string
   */
  public function getCode()
  {
    return $this->code;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param bool
   */
  public function setInSeason($inSeason)
  {
    $this->inSeason = $inSeason;
  }
  /**
   * @return bool
   */
  public function getInSeason()
  {
    return $this->inSeason;
  }
  /**
   * @param IndexInfo
   */
  public function setIndexInfo(IndexInfo $indexInfo)
  {
    $this->indexInfo = $indexInfo;
  }
  /**
   * @return IndexInfo
   */
  public function getIndexInfo()
  {
    return $this->indexInfo;
  }
  /**
   * @param PlantDescription
   */
  public function setPlantDescription(PlantDescription $plantDescription)
  {
    $this->plantDescription = $plantDescription;
  }
  /**
   * @return PlantDescription
   */
  public function getPlantDescription()
  {
    return $this->plantDescription;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PlantInfo::class, 'Google_Service_Pollen_PlantInfo');
