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

namespace Google\Service\Pubsub;

class CloudStorageConfig extends \Google\Model
{
  protected $avroConfigType = AvroConfig::class;
  protected $avroConfigDataType = '';
  /**
   * @var string
   */
  public $bucket;
  /**
   * @var string
   */
  public $filenameDatetimeFormat;
  /**
   * @var string
   */
  public $filenamePrefix;
  /**
   * @var string
   */
  public $filenameSuffix;
  /**
   * @var string
   */
  public $maxBytes;
  /**
   * @var string
   */
  public $maxDuration;
  /**
   * @var string
   */
  public $maxMessages;
  /**
   * @var string
   */
  public $serviceAccountEmail;
  /**
   * @var string
   */
  public $state;
  protected $textConfigType = TextConfig::class;
  protected $textConfigDataType = '';

  /**
   * @param AvroConfig
   */
  public function setAvroConfig(AvroConfig $avroConfig)
  {
    $this->avroConfig = $avroConfig;
  }
  /**
   * @return AvroConfig
   */
  public function getAvroConfig()
  {
    return $this->avroConfig;
  }
  /**
   * @param string
   */
  public function setBucket($bucket)
  {
    $this->bucket = $bucket;
  }
  /**
   * @return string
   */
  public function getBucket()
  {
    return $this->bucket;
  }
  /**
   * @param string
   */
  public function setFilenameDatetimeFormat($filenameDatetimeFormat)
  {
    $this->filenameDatetimeFormat = $filenameDatetimeFormat;
  }
  /**
   * @return string
   */
  public function getFilenameDatetimeFormat()
  {
    return $this->filenameDatetimeFormat;
  }
  /**
   * @param string
   */
  public function setFilenamePrefix($filenamePrefix)
  {
    $this->filenamePrefix = $filenamePrefix;
  }
  /**
   * @return string
   */
  public function getFilenamePrefix()
  {
    return $this->filenamePrefix;
  }
  /**
   * @param string
   */
  public function setFilenameSuffix($filenameSuffix)
  {
    $this->filenameSuffix = $filenameSuffix;
  }
  /**
   * @return string
   */
  public function getFilenameSuffix()
  {
    return $this->filenameSuffix;
  }
  /**
   * @param string
   */
  public function setMaxBytes($maxBytes)
  {
    $this->maxBytes = $maxBytes;
  }
  /**
   * @return string
   */
  public function getMaxBytes()
  {
    return $this->maxBytes;
  }
  /**
   * @param string
   */
  public function setMaxDuration($maxDuration)
  {
    $this->maxDuration = $maxDuration;
  }
  /**
   * @return string
   */
  public function getMaxDuration()
  {
    return $this->maxDuration;
  }
  /**
   * @param string
   */
  public function setMaxMessages($maxMessages)
  {
    $this->maxMessages = $maxMessages;
  }
  /**
   * @return string
   */
  public function getMaxMessages()
  {
    return $this->maxMessages;
  }
  /**
   * @param string
   */
  public function setServiceAccountEmail($serviceAccountEmail)
  {
    $this->serviceAccountEmail = $serviceAccountEmail;
  }
  /**
   * @return string
   */
  public function getServiceAccountEmail()
  {
    return $this->serviceAccountEmail;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param TextConfig
   */
  public function setTextConfig(TextConfig $textConfig)
  {
    $this->textConfig = $textConfig;
  }
  /**
   * @return TextConfig
   */
  public function getTextConfig()
  {
    return $this->textConfig;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudStorageConfig::class, 'Google_Service_Pubsub_CloudStorageConfig');
