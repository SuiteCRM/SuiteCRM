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

namespace Google\Service\SA360;

class GoogleAdsSearchads360V0ResourcesConversionAction extends \Google\Model
{
  /**
   * @var string
   */
  public $appId;
  protected $attributionModelSettingsType = GoogleAdsSearchads360V0ResourcesConversionActionAttributionModelSettings::class;
  protected $attributionModelSettingsDataType = '';
  /**
   * @var string
   */
  public $category;
  /**
   * @var string
   */
  public $clickThroughLookbackWindowDays;
  /**
   * @var string
   */
  public $creationTime;
  protected $floodlightSettingsType = GoogleAdsSearchads360V0ResourcesConversionActionFloodlightSettings::class;
  protected $floodlightSettingsDataType = '';
  /**
   * @var string
   */
  public $id;
  /**
   * @var bool
   */
  public $includeInClientAccountConversionsMetric;
  /**
   * @var bool
   */
  public $includeInConversionsMetric;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $ownerCustomer;
  /**
   * @var bool
   */
  public $primaryForGoal;
  /**
   * @var string
   */
  public $resourceName;
  /**
   * @var string
   */
  public $status;
  /**
   * @var string
   */
  public $type;
  protected $valueSettingsType = GoogleAdsSearchads360V0ResourcesConversionActionValueSettings::class;
  protected $valueSettingsDataType = '';

  /**
   * @param string
   */
  public function setAppId($appId)
  {
    $this->appId = $appId;
  }
  /**
   * @return string
   */
  public function getAppId()
  {
    return $this->appId;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesConversionActionAttributionModelSettings
   */
  public function setAttributionModelSettings(GoogleAdsSearchads360V0ResourcesConversionActionAttributionModelSettings $attributionModelSettings)
  {
    $this->attributionModelSettings = $attributionModelSettings;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesConversionActionAttributionModelSettings
   */
  public function getAttributionModelSettings()
  {
    return $this->attributionModelSettings;
  }
  /**
   * @param string
   */
  public function setCategory($category)
  {
    $this->category = $category;
  }
  /**
   * @return string
   */
  public function getCategory()
  {
    return $this->category;
  }
  /**
   * @param string
   */
  public function setClickThroughLookbackWindowDays($clickThroughLookbackWindowDays)
  {
    $this->clickThroughLookbackWindowDays = $clickThroughLookbackWindowDays;
  }
  /**
   * @return string
   */
  public function getClickThroughLookbackWindowDays()
  {
    return $this->clickThroughLookbackWindowDays;
  }
  /**
   * @param string
   */
  public function setCreationTime($creationTime)
  {
    $this->creationTime = $creationTime;
  }
  /**
   * @return string
   */
  public function getCreationTime()
  {
    return $this->creationTime;
  }
  /**
   * @param GoogleAdsSearchads360V0ResourcesConversionActionFloodlightSettings
   */
  public function setFloodlightSettings(GoogleAdsSearchads360V0ResourcesConversionActionFloodlightSettings $floodlightSettings)
  {
    $this->floodlightSettings = $floodlightSettings;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesConversionActionFloodlightSettings
   */
  public function getFloodlightSettings()
  {
    return $this->floodlightSettings;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
  }
  /**
   * @param bool
   */
  public function setIncludeInClientAccountConversionsMetric($includeInClientAccountConversionsMetric)
  {
    $this->includeInClientAccountConversionsMetric = $includeInClientAccountConversionsMetric;
  }
  /**
   * @return bool
   */
  public function getIncludeInClientAccountConversionsMetric()
  {
    return $this->includeInClientAccountConversionsMetric;
  }
  /**
   * @param bool
   */
  public function setIncludeInConversionsMetric($includeInConversionsMetric)
  {
    $this->includeInConversionsMetric = $includeInConversionsMetric;
  }
  /**
   * @return bool
   */
  public function getIncludeInConversionsMetric()
  {
    return $this->includeInConversionsMetric;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setOwnerCustomer($ownerCustomer)
  {
    $this->ownerCustomer = $ownerCustomer;
  }
  /**
   * @return string
   */
  public function getOwnerCustomer()
  {
    return $this->ownerCustomer;
  }
  /**
   * @param bool
   */
  public function setPrimaryForGoal($primaryForGoal)
  {
    $this->primaryForGoal = $primaryForGoal;
  }
  /**
   * @return bool
   */
  public function getPrimaryForGoal()
  {
    return $this->primaryForGoal;
  }
  /**
   * @param string
   */
  public function setResourceName($resourceName)
  {
    $this->resourceName = $resourceName;
  }
  /**
   * @return string
   */
  public function getResourceName()
  {
    return $this->resourceName;
  }
  /**
   * @param string
   */
  public function setStatus($status)
  {
    $this->status = $status;
  }
  /**
   * @return string
   */
  public function getStatus()
  {
    return $this->status;
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
  /**
   * @param GoogleAdsSearchads360V0ResourcesConversionActionValueSettings
   */
  public function setValueSettings(GoogleAdsSearchads360V0ResourcesConversionActionValueSettings $valueSettings)
  {
    $this->valueSettings = $valueSettings;
  }
  /**
   * @return GoogleAdsSearchads360V0ResourcesConversionActionValueSettings
   */
  public function getValueSettings()
  {
    return $this->valueSettings;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesConversionAction::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesConversionAction');
