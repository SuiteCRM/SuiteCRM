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

namespace Google\Service\Spanner;

class QuorumInfo extends \Google\Model
{
  /**
   * @var string
   */
  public $etag;
  /**
   * @var string
   */
  public $initiator;
  protected $quorumTypeType = QuorumType::class;
  protected $quorumTypeDataType = '';
  /**
   * @var string
   */
  public $startTime;

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
   * @param string
   */
  public function setInitiator($initiator)
  {
    $this->initiator = $initiator;
  }
  /**
   * @return string
   */
  public function getInitiator()
  {
    return $this->initiator;
  }
  /**
   * @param QuorumType
   */
  public function setQuorumType(QuorumType $quorumType)
  {
    $this->quorumType = $quorumType;
  }
  /**
   * @return QuorumType
   */
  public function getQuorumType()
  {
    return $this->quorumType;
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
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(QuorumInfo::class, 'Google_Service_Spanner_QuorumInfo');
