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

class PollenTypeInfo extends \Google\Collection
{
  protected $collection_key = 'healthRecommendations';
  /**
   * @var string
   */
  public $code;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string[]
   */
  public $healthRecommendations;
  /**
   * @var bool
   */
  public $inSeason;
  protected $indexInfoType = IndexInfo::class;
  protected $indexInfoDataType = '';

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
   * @param string[]
   */
  public function setHealthRecommendations($healthRecommendations)
  {
    $this->healthRecommendations = $healthRecommendations;
  }
  /**
   * @return string[]
   */
  public function getHealthRecommendations()
  {
    return $this->healthRecommendations;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PollenTypeInfo::class, 'Google_Service_Pollen_PollenTypeInfo');
