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

class GoogleCloudDiscoveryengineV1PurgeDocumentsRequest extends \Google\Model
{
  protected $errorConfigType = GoogleCloudDiscoveryengineV1PurgeErrorConfig::class;
  protected $errorConfigDataType = '';
  /**
   * @var string
   */
  public $filter;
  /**
   * @var bool
   */
  public $force;
  protected $gcsSourceType = GoogleCloudDiscoveryengineV1GcsSource::class;
  protected $gcsSourceDataType = '';
  protected $inlineSourceType = GoogleCloudDiscoveryengineV1PurgeDocumentsRequestInlineSource::class;
  protected $inlineSourceDataType = '';

  /**
   * @param GoogleCloudDiscoveryengineV1PurgeErrorConfig
   */
  public function setErrorConfig(GoogleCloudDiscoveryengineV1PurgeErrorConfig $errorConfig)
  {
    $this->errorConfig = $errorConfig;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1PurgeErrorConfig
   */
  public function getErrorConfig()
  {
    return $this->errorConfig;
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
   * @param bool
   */
  public function setForce($force)
  {
    $this->force = $force;
  }
  /**
   * @return bool
   */
  public function getForce()
  {
    return $this->force;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1GcsSource
   */
  public function setGcsSource(GoogleCloudDiscoveryengineV1GcsSource $gcsSource)
  {
    $this->gcsSource = $gcsSource;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1GcsSource
   */
  public function getGcsSource()
  {
    return $this->gcsSource;
  }
  /**
   * @param GoogleCloudDiscoveryengineV1PurgeDocumentsRequestInlineSource
   */
  public function setInlineSource(GoogleCloudDiscoveryengineV1PurgeDocumentsRequestInlineSource $inlineSource)
  {
    $this->inlineSource = $inlineSource;
  }
  /**
   * @return GoogleCloudDiscoveryengineV1PurgeDocumentsRequestInlineSource
   */
  public function getInlineSource()
  {
    return $this->inlineSource;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDiscoveryengineV1PurgeDocumentsRequest::class, 'Google_Service_DiscoveryEngine_GoogleCloudDiscoveryengineV1PurgeDocumentsRequest');
