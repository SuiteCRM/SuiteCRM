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

class StagePerformanceStandaloneInsight extends \Google\Collection
{
  protected $collection_key = 'highCardinalityJoins';
  protected $biEngineReasonsType = BiEngineReason::class;
  protected $biEngineReasonsDataType = 'array';
  protected $highCardinalityJoinsType = HighCardinalityJoin::class;
  protected $highCardinalityJoinsDataType = 'array';
  /**
   * @var bool
   */
  public $insufficientShuffleQuota;
  protected $partitionSkewType = PartitionSkew::class;
  protected $partitionSkewDataType = '';
  /**
   * @var bool
   */
  public $slotContention;
  /**
   * @var string
   */
  public $stageId;

  /**
   * @param BiEngineReason[]
   */
  public function setBiEngineReasons($biEngineReasons)
  {
    $this->biEngineReasons = $biEngineReasons;
  }
  /**
   * @return BiEngineReason[]
   */
  public function getBiEngineReasons()
  {
    return $this->biEngineReasons;
  }
  /**
   * @param HighCardinalityJoin[]
   */
  public function setHighCardinalityJoins($highCardinalityJoins)
  {
    $this->highCardinalityJoins = $highCardinalityJoins;
  }
  /**
   * @return HighCardinalityJoin[]
   */
  public function getHighCardinalityJoins()
  {
    return $this->highCardinalityJoins;
  }
  /**
   * @param bool
   */
  public function setInsufficientShuffleQuota($insufficientShuffleQuota)
  {
    $this->insufficientShuffleQuota = $insufficientShuffleQuota;
  }
  /**
   * @return bool
   */
  public function getInsufficientShuffleQuota()
  {
    return $this->insufficientShuffleQuota;
  }
  /**
   * @param PartitionSkew
   */
  public function setPartitionSkew(PartitionSkew $partitionSkew)
  {
    $this->partitionSkew = $partitionSkew;
  }
  /**
   * @return PartitionSkew
   */
  public function getPartitionSkew()
  {
    return $this->partitionSkew;
  }
  /**
   * @param bool
   */
  public function setSlotContention($slotContention)
  {
    $this->slotContention = $slotContention;
  }
  /**
   * @return bool
   */
  public function getSlotContention()
  {
    return $this->slotContention;
  }
  /**
   * @param string
   */
  public function setStageId($stageId)
  {
    $this->stageId = $stageId;
  }
  /**
   * @return string
   */
  public function getStageId()
  {
    return $this->stageId;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(StagePerformanceStandaloneInsight::class, 'Google_Service_Bigquery_StagePerformanceStandaloneInsight');
