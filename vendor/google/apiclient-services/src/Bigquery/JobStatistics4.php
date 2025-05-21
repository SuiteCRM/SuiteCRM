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

namespace Google\Service\Bigquery;

class JobStatistics4 extends \Google\Collection
{
  protected $collection_key = 'timeline';
  /**
   * @var string[]
   */
  public $destinationUriFileCounts;
  /**
   * @var string
   */
  public $inputBytes;
  protected $timelineType = QueryTimelineSample::class;
  protected $timelineDataType = 'array';

  /**
   * @param string[]
   */
  public function setDestinationUriFileCounts($destinationUriFileCounts)
  {
    $this->destinationUriFileCounts = $destinationUriFileCounts;
  }
  /**
   * @return string[]
   */
  public function getDestinationUriFileCounts()
  {
    return $this->destinationUriFileCounts;
  }
  /**
   * @param string
   */
  public function setInputBytes($inputBytes)
  {
    $this->inputBytes = $inputBytes;
  }
  /**
   * @return string
   */
  public function getInputBytes()
  {
    return $this->inputBytes;
  }
  /**
   * @param QueryTimelineSample[]
   */
  public function setTimeline($timeline)
  {
    $this->timeline = $timeline;
  }
  /**
   * @return QueryTimelineSample[]
   */
  public function getTimeline()
  {
    return $this->timeline;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(JobStatistics4::class, 'Google_Service_Bigquery_JobStatistics4');
