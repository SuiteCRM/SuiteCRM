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

namespace Google\Service\Dialogflow;

class GoogleCloudDialogflowCxV3QueryParameters extends \Google\Collection
{
  protected $collection_key = 'sessionEntityTypes';
  /**
   * @var bool
   */
  public $analyzeQueryTextSentiment;
  /**
   * @var string
   */
  public $channel;
  /**
   * @var string
   */
  public $currentPage;
  /**
   * @var bool
   */
  public $disableWebhook;
  /**
   * @var array[]
   */
  public $endUserMetadata;
  /**
   * @var string[]
   */
  public $flowVersions;
  protected $geoLocationType = GoogleTypeLatLng::class;
  protected $geoLocationDataType = '';
  /**
   * @var array[]
   */
  public $parameters;
  /**
   * @var array[]
   */
  public $payload;
  /**
   * @var bool
   */
  public $populateDataStoreConnectionSignals;
  protected $searchConfigType = GoogleCloudDialogflowCxV3SearchConfig::class;
  protected $searchConfigDataType = '';
  protected $sessionEntityTypesType = GoogleCloudDialogflowCxV3SessionEntityType::class;
  protected $sessionEntityTypesDataType = 'array';
  /**
   * @var string
   */
  public $sessionTtl;
  /**
   * @var string
   */
  public $timeZone;
  /**
   * @var string[]
   */
  public $webhookHeaders;

  /**
   * @param bool
   */
  public function setAnalyzeQueryTextSentiment($analyzeQueryTextSentiment)
  {
    $this->analyzeQueryTextSentiment = $analyzeQueryTextSentiment;
  }
  /**
   * @return bool
   */
  public function getAnalyzeQueryTextSentiment()
  {
    return $this->analyzeQueryTextSentiment;
  }
  /**
   * @param string
   */
  public function setChannel($channel)
  {
    $this->channel = $channel;
  }
  /**
   * @return string
   */
  public function getChannel()
  {
    return $this->channel;
  }
  /**
   * @param string
   */
  public function setCurrentPage($currentPage)
  {
    $this->currentPage = $currentPage;
  }
  /**
   * @return string
   */
  public function getCurrentPage()
  {
    return $this->currentPage;
  }
  /**
   * @param bool
   */
  public function setDisableWebhook($disableWebhook)
  {
    $this->disableWebhook = $disableWebhook;
  }
  /**
   * @return bool
   */
  public function getDisableWebhook()
  {
    return $this->disableWebhook;
  }
  /**
   * @param array[]
   */
  public function setEndUserMetadata($endUserMetadata)
  {
    $this->endUserMetadata = $endUserMetadata;
  }
  /**
   * @return array[]
   */
  public function getEndUserMetadata()
  {
    return $this->endUserMetadata;
  }
  /**
   * @param string[]
   */
  public function setFlowVersions($flowVersions)
  {
    $this->flowVersions = $flowVersions;
  }
  /**
   * @return string[]
   */
  public function getFlowVersions()
  {
    return $this->flowVersions;
  }
  /**
   * @param GoogleTypeLatLng
   */
  public function setGeoLocation(GoogleTypeLatLng $geoLocation)
  {
    $this->geoLocation = $geoLocation;
  }
  /**
   * @return GoogleTypeLatLng
   */
  public function getGeoLocation()
  {
    return $this->geoLocation;
  }
  /**
   * @param array[]
   */
  public function setParameters($parameters)
  {
    $this->parameters = $parameters;
  }
  /**
   * @return array[]
   */
  public function getParameters()
  {
    return $this->parameters;
  }
  /**
   * @param array[]
   */
  public function setPayload($payload)
  {
    $this->payload = $payload;
  }
  /**
   * @return array[]
   */
  public function getPayload()
  {
    return $this->payload;
  }
  /**
   * @param bool
   */
  public function setPopulateDataStoreConnectionSignals($populateDataStoreConnectionSignals)
  {
    $this->populateDataStoreConnectionSignals = $populateDataStoreConnectionSignals;
  }
  /**
   * @return bool
   */
  public function getPopulateDataStoreConnectionSignals()
  {
    return $this->populateDataStoreConnectionSignals;
  }
  /**
   * @param GoogleCloudDialogflowCxV3SearchConfig
   */
  public function setSearchConfig(GoogleCloudDialogflowCxV3SearchConfig $searchConfig)
  {
    $this->searchConfig = $searchConfig;
  }
  /**
   * @return GoogleCloudDialogflowCxV3SearchConfig
   */
  public function getSearchConfig()
  {
    return $this->searchConfig;
  }
  /**
   * @param GoogleCloudDialogflowCxV3SessionEntityType[]
   */
  public function setSessionEntityTypes($sessionEntityTypes)
  {
    $this->sessionEntityTypes = $sessionEntityTypes;
  }
  /**
   * @return GoogleCloudDialogflowCxV3SessionEntityType[]
   */
  public function getSessionEntityTypes()
  {
    return $this->sessionEntityTypes;
  }
  /**
   * @param string
   */
  public function setSessionTtl($sessionTtl)
  {
    $this->sessionTtl = $sessionTtl;
  }
  /**
   * @return string
   */
  public function getSessionTtl()
  {
    return $this->sessionTtl;
  }
  /**
   * @param string
   */
  public function setTimeZone($timeZone)
  {
    $this->timeZone = $timeZone;
  }
  /**
   * @return string
   */
  public function getTimeZone()
  {
    return $this->timeZone;
  }
  /**
   * @param string[]
   */
  public function setWebhookHeaders($webhookHeaders)
  {
    $this->webhookHeaders = $webhookHeaders;
  }
  /**
   * @return string[]
   */
  public function getWebhookHeaders()
  {
    return $this->webhookHeaders;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDialogflowCxV3QueryParameters::class, 'Google_Service_Dialogflow_GoogleCloudDialogflowCxV3QueryParameters');
