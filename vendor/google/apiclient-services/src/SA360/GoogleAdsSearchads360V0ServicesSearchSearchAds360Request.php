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

class GoogleAdsSearchads360V0ServicesSearchSearchAds360Request extends \Google\Model
{
  /**
   * @var int
   */
  public $pageSize;
  /**
   * @var string
   */
  public $pageToken;
  /**
   * @var string
   */
  public $query;
  /**
   * @var bool
   */
  public $returnTotalResultsCount;
  /**
   * @var string
   */
  public $summaryRowSetting;
  /**
   * @var bool
   */
  public $validateOnly;

  /**
   * @param int
   */
  public function setPageSize($pageSize)
  {
    $this->pageSize = $pageSize;
  }
  /**
   * @return int
   */
  public function getPageSize()
  {
    return $this->pageSize;
  }
  /**
   * @param string
   */
  public function setPageToken($pageToken)
  {
    $this->pageToken = $pageToken;
  }
  /**
   * @return string
   */
  public function getPageToken()
  {
    return $this->pageToken;
  }
  /**
   * @param string
   */
  public function setQuery($query)
  {
    $this->query = $query;
  }
  /**
   * @return string
   */
  public function getQuery()
  {
    return $this->query;
  }
  /**
   * @param bool
   */
  public function setReturnTotalResultsCount($returnTotalResultsCount)
  {
    $this->returnTotalResultsCount = $returnTotalResultsCount;
  }
  /**
   * @return bool
   */
  public function getReturnTotalResultsCount()
  {
    return $this->returnTotalResultsCount;
  }
  /**
   * @param string
   */
  public function setSummaryRowSetting($summaryRowSetting)
  {
    $this->summaryRowSetting = $summaryRowSetting;
  }
  /**
   * @return string
   */
  public function getSummaryRowSetting()
  {
    return $this->summaryRowSetting;
  }
  /**
   * @param bool
   */
  public function setValidateOnly($validateOnly)
  {
    $this->validateOnly = $validateOnly;
  }
  /**
   * @return bool
   */
  public function getValidateOnly()
  {
    return $this->validateOnly;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleAdsSearchads360V0ServicesSearchSearchAds360Request::class, 'Google_Service_SA360_GoogleAdsSearchads360V0ServicesSearchSearchAds360Request');
