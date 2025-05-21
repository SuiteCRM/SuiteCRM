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

namespace Google\Service\Eventarc;

class Pipeline extends \Google\Collection
{
  protected $collection_key = 'mediations';
  /**
   * @var string[]
   */
  public $annotations;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $cryptoKeyName;
  protected $destinationsType = GoogleCloudEventarcV1PipelineDestination::class;
  protected $destinationsDataType = 'array';
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $etag;
  protected $inputPayloadFormatType = GoogleCloudEventarcV1PipelineMessagePayloadFormat::class;
  protected $inputPayloadFormatDataType = '';
  /**
   * @var string[]
   */
  public $labels;
  protected $loggingConfigType = LoggingConfig::class;
  protected $loggingConfigDataType = '';
  protected $mediationsType = GoogleCloudEventarcV1PipelineMediation::class;
  protected $mediationsDataType = 'array';
  /**
   * @var string
   */
  public $name;
  protected $retryPolicyType = GoogleCloudEventarcV1PipelineRetryPolicy::class;
  protected $retryPolicyDataType = '';
  /**
   * @var string
   */
  public $uid;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string[]
   */
  public function setAnnotations($annotations)
  {
    $this->annotations = $annotations;
  }
  /**
   * @return string[]
   */
  public function getAnnotations()
  {
    return $this->annotations;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setCryptoKeyName($cryptoKeyName)
  {
    $this->cryptoKeyName = $cryptoKeyName;
  }
  /**
   * @return string
   */
  public function getCryptoKeyName()
  {
    return $this->cryptoKeyName;
  }
  /**
   * @param GoogleCloudEventarcV1PipelineDestination[]
   */
  public function setDestinations($destinations)
  {
    $this->destinations = $destinations;
  }
  /**
   * @return GoogleCloudEventarcV1PipelineDestination[]
   */
  public function getDestinations()
  {
    return $this->destinations;
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
   * @param string
   */
  public function setEtag($etag)
  {
    $this->etag = $etag;
  }
  /**
   * @return string
   */
  public function getEtag()
  {
    return $this->etag;
  }
  /**
   * @param GoogleCloudEventarcV1PipelineMessagePayloadFormat
   */
  public function setInputPayloadFormat(GoogleCloudEventarcV1PipelineMessagePayloadFormat $inputPayloadFormat)
  {
    $this->inputPayloadFormat = $inputPayloadFormat;
  }
  /**
   * @return GoogleCloudEventarcV1PipelineMessagePayloadFormat
   */
  public function getInputPayloadFormat()
  {
    return $this->inputPayloadFormat;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param LoggingConfig
   */
  public function setLoggingConfig(LoggingConfig $loggingConfig)
  {
    $this->loggingConfig = $loggingConfig;
  }
  /**
   * @return LoggingConfig
   */
  public function getLoggingConfig()
  {
    return $this->loggingConfig;
  }
  /**
   * @param GoogleCloudEventarcV1PipelineMediation[]
   */
  public function setMediations($mediations)
  {
    $this->mediations = $mediations;
  }
  /**
   * @return GoogleCloudEventarcV1PipelineMediation[]
   */
  public function getMediations()
  {
    return $this->mediations;
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
   * @param GoogleCloudEventarcV1PipelineRetryPolicy
   */
  public function setRetryPolicy(GoogleCloudEventarcV1PipelineRetryPolicy $retryPolicy)
  {
    $this->retryPolicy = $retryPolicy;
  }
  /**
   * @return GoogleCloudEventarcV1PipelineRetryPolicy
   */
  public function getRetryPolicy()
  {
    return $this->retryPolicy;
  }
  /**
   * @param string
   */
  public function setUid($uid)
  {
    $this->uid = $uid;
  }
  /**
   * @return string
   */
  public function getUid()
  {
    return $this->uid;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Pipeline::class, 'Google_Service_Eventarc_Pipeline');
