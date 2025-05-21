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

namespace Google\Service\Networkconnectivity;

class LinkedVpcNetwork extends \Google\Collection
{
  protected $collection_key = 'producerVpcSpokes';
  /**
   * @var string[]
   */
  public $excludeExportRanges;
  /**
   * @var string[]
   */
  public $includeExportRanges;
  /**
   * @var string[]
   */
  public $producerVpcSpokes;
  /**
   * @var string
   */
  public $uri;

  /**
   * @param string[]
   */
  public function setExcludeExportRanges($excludeExportRanges)
  {
    $this->excludeExportRanges = $excludeExportRanges;
  }
  /**
   * @return string[]
   */
  public function getExcludeExportRanges()
  {
    return $this->excludeExportRanges;
  }
  /**
   * @param string[]
   */
  public function setIncludeExportRanges($includeExportRanges)
  {
    $this->includeExportRanges = $includeExportRanges;
  }
  /**
   * @return string[]
   */
  public function getIncludeExportRanges()
  {
    return $this->includeExportRanges;
  }
  /**
   * @param string[]
   */
  public function setProducerVpcSpokes($producerVpcSpokes)
  {
    $this->producerVpcSpokes = $producerVpcSpokes;
  }
  /**
   * @return string[]
   */
  public function getProducerVpcSpokes()
  {
    return $this->producerVpcSpokes;
  }
  /**
   * @param string
   */
  public function setUri($uri)
  {
    $this->uri = $uri;
  }
  /**
   * @return string
   */
  public function getUri()
  {
    return $this->uri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(LinkedVpcNetwork::class, 'Google_Service_Networkconnectivity_LinkedVpcNetwork');
