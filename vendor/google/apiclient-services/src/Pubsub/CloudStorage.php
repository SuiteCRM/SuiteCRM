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

class CloudStorage extends \Google\Model
{
  protected $avroFormatType = AvroFormat::class;
  protected $avroFormatDataType = '';
  /**
   * @var string
   */
  public $bucket;
  /**
   * @var string
   */
  public $matchGlob;
  /**
   * @var string
   */
  public $minimumObjectCreateTime;
  protected $pubsubAvroFormatType = PubSubAvroFormat::class;
  protected $pubsubAvroFormatDataType = '';
  /**
   * @var string
   */
  public $state;
  protected $textFormatType = TextFormat::class;
  protected $textFormatDataType = '';

  /**
   * @param AvroFormat
   */
  public function setAvroFormat(AvroFormat $avroFormat)
  {
    $this->avroFormat = $avroFormat;
  }
  /**
   * @return AvroFormat
   */
  public function getAvroFormat()
  {
    return $this->avroFormat;
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
  public function setMatchGlob($matchGlob)
  {
    $this->matchGlob = $matchGlob;
  }
  /**
   * @return string
   */
  public function getMatchGlob()
  {
    return $this->matchGlob;
  }
  /**
   * @param string
   */
  public function setMinimumObjectCreateTime($minimumObjectCreateTime)
  {
    $this->minimumObjectCreateTime = $minimumObjectCreateTime;
  }
  /**
   * @return string
   */
  public function getMinimumObjectCreateTime()
  {
    return $this->minimumObjectCreateTime;
  }
  /**
   * @param PubSubAvroFormat
   */
  public function setPubsubAvroFormat(PubSubAvroFormat $pubsubAvroFormat)
  {
    $this->pubsubAvroFormat = $pubsubAvroFormat;
  }
  /**
   * @return PubSubAvroFormat
   */
  public function getPubsubAvroFormat()
  {
    return $this->pubsubAvroFormat;
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
   * @param TextFormat
   */
  public function setTextFormat(TextFormat $textFormat)
  {
    $this->textFormat = $textFormat;
  }
  /**
   * @return TextFormat
   */
  public function getTextFormat()
  {
    return $this->textFormat;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudStorage::class, 'Google_Service_Pubsub_CloudStorage');
