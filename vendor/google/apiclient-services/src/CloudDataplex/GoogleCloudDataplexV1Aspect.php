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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1Aspect extends \Google\Model
{
  protected $aspectSourceType = GoogleCloudDataplexV1AspectSource::class;
  protected $aspectSourceDataType = '';
  /**
   * @var string
   */
  public $aspectType;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var array[]
   */
  public $data;
  /**
   * @var string
   */
  public $path;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param GoogleCloudDataplexV1AspectSource
   */
  public function setAspectSource(GoogleCloudDataplexV1AspectSource $aspectSource)
  {
    $this->aspectSource = $aspectSource;
  }
  /**
   * @return GoogleCloudDataplexV1AspectSource
   */
  public function getAspectSource()
  {
    return $this->aspectSource;
  }
  /**
   * @param string
   */
  public function setAspectType($aspectType)
  {
    $this->aspectType = $aspectType;
  }
  /**
   * @return string
   */
  public function getAspectType()
  {
    return $this->aspectType;
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
   * @param array[]
   */
  public function setData($data)
  {
    $this->data = $data;
  }
  /**
   * @return array[]
   */
  public function getData()
  {
    return $this->data;
  }
  /**
   * @param string
   */
  public function setPath($path)
  {
    $this->path = $path;
  }
  /**
   * @return string
   */
  public function getPath()
  {
    return $this->path;
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
class_alias(GoogleCloudDataplexV1Aspect::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1Aspect');
