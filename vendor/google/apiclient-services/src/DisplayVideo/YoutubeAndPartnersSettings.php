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

namespace Google\Service\DisplayVideo;

class YoutubeAndPartnersSettings extends \Google\Collection
{
  protected $collection_key = 'relatedVideoIds';
  /**
   * @var string
   */
  public $contentCategory;
  /**
   * @var string
   */
  public $effectiveContentCategory;
  protected $inventorySourceSettingsType = YoutubeAndPartnersInventorySourceConfig::class;
  protected $inventorySourceSettingsDataType = '';
  /**
   * @var string
   */
  public $leadFormId;
  /**
   * @var string
   */
  public $linkedMerchantId;
  /**
   * @var string[]
   */
  public $relatedVideoIds;
  protected $targetFrequencyType = TargetFrequency::class;
  protected $targetFrequencyDataType = '';
  protected $thirdPartyMeasurementConfigsType = ThirdPartyMeasurementConfigs::class;
  protected $thirdPartyMeasurementConfigsDataType = '';
  protected $videoAdSequenceSettingsType = VideoAdSequenceSettings::class;
  protected $videoAdSequenceSettingsDataType = '';
  protected $viewFrequencyCapType = FrequencyCap::class;
  protected $viewFrequencyCapDataType = '';

  /**
   * @param string
   */
  public function setContentCategory($contentCategory)
  {
    $this->contentCategory = $contentCategory;
  }
  /**
   * @return string
   */
  public function getContentCategory()
  {
    return $this->contentCategory;
  }
  /**
   * @param string
   */
  public function setEffectiveContentCategory($effectiveContentCategory)
  {
    $this->effectiveContentCategory = $effectiveContentCategory;
  }
  /**
   * @return string
   */
  public function getEffectiveContentCategory()
  {
    return $this->effectiveContentCategory;
  }
  /**
   * @param YoutubeAndPartnersInventorySourceConfig
   */
  public function setInventorySourceSettings(YoutubeAndPartnersInventorySourceConfig $inventorySourceSettings)
  {
    $this->inventorySourceSettings = $inventorySourceSettings;
  }
  /**
   * @return YoutubeAndPartnersInventorySourceConfig
   */
  public function getInventorySourceSettings()
  {
    return $this->inventorySourceSettings;
  }
  /**
   * @param string
   */
  public function setLeadFormId($leadFormId)
  {
    $this->leadFormId = $leadFormId;
  }
  /**
   * @return string
   */
  public function getLeadFormId()
  {
    return $this->leadFormId;
  }
  /**
   * @param string
   */
  public function setLinkedMerchantId($linkedMerchantId)
  {
    $this->linkedMerchantId = $linkedMerchantId;
  }
  /**
   * @return string
   */
  public function getLinkedMerchantId()
  {
    return $this->linkedMerchantId;
  }
  /**
   * @param string[]
   */
  public function setRelatedVideoIds($relatedVideoIds)
  {
    $this->relatedVideoIds = $relatedVideoIds;
  }
  /**
   * @return string[]
   */
  public function getRelatedVideoIds()
  {
    return $this->relatedVideoIds;
  }
  /**
   * @param TargetFrequency
   */
  public function setTargetFrequency(TargetFrequency $targetFrequency)
  {
    $this->targetFrequency = $targetFrequency;
  }
  /**
   * @return TargetFrequency
   */
  public function getTargetFrequency()
  {
    return $this->targetFrequency;
  }
  /**
   * @param ThirdPartyMeasurementConfigs
   */
  public function setThirdPartyMeasurementConfigs(ThirdPartyMeasurementConfigs $thirdPartyMeasurementConfigs)
  {
    $this->thirdPartyMeasurementConfigs = $thirdPartyMeasurementConfigs;
  }
  /**
   * @return ThirdPartyMeasurementConfigs
   */
  public function getThirdPartyMeasurementConfigs()
  {
    return $this->thirdPartyMeasurementConfigs;
  }
  /**
   * @param VideoAdSequenceSettings
   */
  public function setVideoAdSequenceSettings(VideoAdSequenceSettings $videoAdSequenceSettings)
  {
    $this->videoAdSequenceSettings = $videoAdSequenceSettings;
  }
  /**
   * @return VideoAdSequenceSettings
   */
  public function getVideoAdSequenceSettings()
  {
    return $this->videoAdSequenceSettings;
  }
  /**
   * @param FrequencyCap
   */
  public function setViewFrequencyCap(FrequencyCap $viewFrequencyCap)
  {
    $this->viewFrequencyCap = $viewFrequencyCap;
  }
  /**
   * @return FrequencyCap
   */
  public function getViewFrequencyCap()
  {
    return $this->viewFrequencyCap;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(YoutubeAndPartnersSettings::class, 'Google_Service_DisplayVideo_YoutubeAndPartnersSettings');
