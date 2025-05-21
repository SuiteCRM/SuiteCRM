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

class GoogleAdsSearchads360V0ResourcesAssetGroupSignal extends \Google\Model
{
  /**
   * @var string
   */
  public $assetGroup;
  protected $audienceType = GoogleAdsSearchads360V0CommonAudienceInfo::class;
  protected $audienceDataType = '';
  /**
   * @var string
   */
  public $resourceName;

  /**
   * @param string
   */
  public function setAssetGroup($assetGroup)
  {
    $this->assetGroup = $assetGroup;
  }
  /**
   * @return string
   */
  public function getAssetGroup()
  {
    return $this->assetGroup;
  }
  /**
   * @param GoogleAdsSearchads360V0CommonAudienceInfo
   */
  public function setAudience(GoogleAdsSearchads360V0CommonAudienceInfo $audience)
  {
    $this->audience = $audience;
  }
  /**
   * @return GoogleAdsSearchads360V0CommonAudienceInfo
   */
  public function getAudience()
  {
    return $this->audience;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesAssetGroupSignal::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesAssetGroupSignal');
