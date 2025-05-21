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

class GoogleCloudDataplexV1DataScanEventDataQualityResult extends \Google\Model
{
  /**
   * @var float[]
   */
  public $columnScore;
  /**
   * @var bool[]
   */
  public $dimensionPassed;
  /**
   * @var float[]
   */
  public $dimensionScore;
  /**
   * @var bool
   */
  public $passed;
  /**
   * @var string
   */
  public $rowCount;
  /**
   * @var float
   */
  public $score;

  /**
   * @param float[]
   */
  public function setColumnScore($columnScore)
  {
    $this->columnScore = $columnScore;
  }
  /**
   * @return float[]
   */
  public function getColumnScore()
  {
    return $this->columnScore;
  }
  /**
   * @param bool[]
   */
  public function setDimensionPassed($dimensionPassed)
  {
    $this->dimensionPassed = $dimensionPassed;
  }
  /**
   * @return bool[]
   */
  public function getDimensionPassed()
  {
    return $this->dimensionPassed;
  }
  /**
   * @param float[]
   */
  public function setDimensionScore($dimensionScore)
  {
    $this->dimensionScore = $dimensionScore;
  }
  /**
   * @return float[]
   */
  public function getDimensionScore()
  {
    return $this->dimensionScore;
  }
  /**
   * @param bool
   */
  public function setPassed($passed)
  {
    $this->passed = $passed;
  }
  /**
   * @return bool
   */
  public function getPassed()
  {
    return $this->passed;
  }
  /**
   * @param string
   */
  public function setRowCount($rowCount)
  {
    $this->rowCount = $rowCount;
  }
  /**
   * @return string
   */
  public function getRowCount()
  {
    return $this->rowCount;
  }
  /**
   * @param float
   */
  public function setScore($score)
  {
    $this->score = $score;
  }
  /**
   * @return float
   */
  public function getScore()
  {
    return $this->score;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1DataScanEventDataQualityResult::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1DataScanEventDataQualityResult');
