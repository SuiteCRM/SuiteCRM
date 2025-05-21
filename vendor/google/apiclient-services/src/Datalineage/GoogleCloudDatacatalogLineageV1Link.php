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

namespace Google\Service\Datalineage;

class GoogleCloudDatacatalogLineageV1Link extends \Google\Model
{
  /**
   * @var string
   */
  public $endTime;
  /**
   * @var string
   */
  public $name;
  protected $sourceType = GoogleCloudDatacatalogLineageV1EntityReference::class;
  protected $sourceDataType = '';
  /**
   * @var string
   */
  public $startTime;
  protected $targetType = GoogleCloudDatacatalogLineageV1EntityReference::class;
  protected $targetDataType = '';

  /**
   * @param string
   */
  public function setEndTime($endTime)
  {
    $this->endTime = $endTime;
  }
  /**
   * @return string
   */
  public function getEndTime()
  {
    return $this->endTime;
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
   * @param GoogleCloudDatacatalogLineageV1EntityReference
   */
  public function setSource(GoogleCloudDatacatalogLineageV1EntityReference $source)
  {
    $this->source = $source;
  }
  /**
   * @return GoogleCloudDatacatalogLineageV1EntityReference
   */
  public function getSource()
  {
    return $this->source;
  }
  /**
   * @param string
   */
  public function setStartTime($startTime)
  {
    $this->startTime = $startTime;
  }
  /**
   * @return string
   */
  public function getStartTime()
  {
    return $this->startTime;
  }
  /**
   * @param GoogleCloudDatacatalogLineageV1EntityReference
   */
  public function setTarget(GoogleCloudDatacatalogLineageV1EntityReference $target)
  {
    $this->target = $target;
  }
  /**
   * @return GoogleCloudDatacatalogLineageV1EntityReference
   */
  public function getTarget()
  {
    return $this->target;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDatacatalogLineageV1Link::class, 'Google_Service_Datalineage_GoogleCloudDatacatalogLineageV1Link');
