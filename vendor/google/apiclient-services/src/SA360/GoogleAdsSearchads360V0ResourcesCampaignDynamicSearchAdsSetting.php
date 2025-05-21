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

class GoogleAdsSearchads360V0ResourcesCampaignDynamicSearchAdsSetting extends \Google\Model
{
  /**
   * @var string
   */
  public $domainName;
  /**
   * @var string
   */
  public $languageCode;
  /**
   * @var bool
   */
  public $useSuppliedUrlsOnly;

  /**
   * @param string
   */
  public function setDomainName($domainName)
  {
    $this->domainName = $domainName;
  }
  /**
   * @return string
   */
  public function getDomainName()
  {
    return $this->domainName;
  }
  /**
   * @param string
   */
  public function setLanguageCode($languageCode)
  {
    $this->languageCode = $languageCode;
  }
  /**
   * @return string
   */
  public function getLanguageCode()
  {
    return $this->languageCode;
  }
  /**
   * @param bool
   */
  public function setUseSuppliedUrlsOnly($useSuppliedUrlsOnly)
  {
    $this->useSuppliedUrlsOnly = $useSuppliedUrlsOnly;
  }
  /**
   * @return bool
   */
  public function getUseSuppliedUrlsOnly()
  {
    return $this->useSuppliedUrlsOnly;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ResourcesCampaignDynamicSearchAdsSetting::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ResourcesCampaignDynamicSearchAdsSetting');
