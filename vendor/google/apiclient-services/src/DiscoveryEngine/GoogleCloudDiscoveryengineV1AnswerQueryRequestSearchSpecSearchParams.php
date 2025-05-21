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

namespace Google\Service\DiscoveryEngine;

class GoogleCloudDiscoveryengineV1AnswerQueryRequestSearchSpecSearchParams extends \Google\Collection
{
  protected $collection_key = 'dataStoreSpecs';
  protected $boostSpecType = GoogleCloudDiscoveryengineV1SearchRequestBoostSpec::class;
  protected $boostSpecDataType = '';
  protected $dataStoreSpecsType = GoogleCloudDiscoveryengineV1SearchRequestDataStoreSpec::class;
  protected $dataStoreSpecsDataType = 'array';
  /**
   * @var string
   */
  public $filter;
  /**
   * @var int
   */
  public $maxReturnResults;
  /**
   * @var string
   */
  public $orderBy;
  /**
   * @var string
   */
  public $searchResultMode;

  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestBoostSpec
   */
  public function setBoostSpec(GoogleCloudDiscoveryengineV1SearchRequestBoostSpec $boostSpec)
  {
    $this->boostSpec = $boostSpec;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestBoostSpec
   */
  public function getBoostSpec()
  {
    return $this->boostSpec;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1SearchRequestDataStoreSpec[]
   */
  public function setDataStoreSpecs($dataStoreSpecs)
  {
    $this->dataStoreSpecs = $dataStoreSpecs;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1SearchRequestDataStoreSpec[]
   */
  public function getDataStoreSpecs()
  {
    return $this->dataStoreSpecs;
  }
  /**
   * @param string
   */
  public function setFilter($filter)
  {
    $this->filter = $filter;
  }
  /**
   * @return string
   */
  public function getFilter()
  {
    return $this->filter;
  }
  /**
   * @param int
   */
  public function setMaxReturnResults($maxReturnResults)
  {
    $this->maxReturnResults = $maxReturnResults;
  }
  /**
   * @return int
   */
  public function getMaxReturnResults()
  {
    return $this->maxReturnResults;
  }
  /**
   * @param string
   */
  public function setOrderBy($orderBy)
  {
    $this->orderBy = $orderBy;
  }
  /**
   * @return string
   */
  public function getOrderBy()
  {
    return $this->orderBy;
  }
  /**
   * @param string
   */
  public function setSearchResultMode($searchResultMode)
  {
    $this->searchResultMode = $searchResultMode;
  }
  /**
   * @return string
   */
  public function getSearchResultMode()
  {
    return $this->searchResultMode;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1AnswerQueryRequestSearchSpecSearchParams::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1AnswerQueryRequestSearchSpecSearchParams');
