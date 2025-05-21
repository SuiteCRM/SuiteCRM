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

namespace Google\Service\Datastream;

class CdcStrategy extends \Google\Model
{
  protected $mostRecentStartPositionType = MostRecentStartPosition::class;
  protected $mostRecentStartPositionDataType = '';
  protected $nextAvailableStartPositionType = NextAvailableStartPosition::class;
  protected $nextAvailableStartPositionDataType = '';
  protected $specificStartPositionType = SpecificStartPosition::class;
  protected $specificStartPositionDataType = '';

  /**
   * @param MostRecentStartPosition
   */
  public function setMostRecentStartPosition(MostRecentStartPosition $mostRecentStartPosition)
  {
    $this->mostRecentStartPosition = $mostRecentStartPosition;
  }
  /**
   * @return MostRecentStartPosition
   */
  public function getMostRecentStartPosition()
  {
    return $this->mostRecentStartPosition;
  }
  /**
   * @param NextAvailableStartPosition
   */
  public function setNextAvailableStartPosition(NextAvailableStartPosition $nextAvailableStartPosition)
  {
    $this->nextAvailableStartPosition = $nextAvailableStartPosition;
  }
  /**
   * @return NextAvailableStartPosition
   */
  public function getNextAvailableStartPosition()
  {
    return $this->nextAvailableStartPosition;
  }
  /**
   * @param SpecificStartPosition
   */
  public function setSpecificStartPosition(SpecificStartPosition $specificStartPosition)
  {
    $this->specificStartPosition = $specificStartPosition;
  }
  /**
   * @return SpecificStartPosition
   */
  public function getSpecificStartPosition()
  {
    return $this->specificStartPosition;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CdcStrategy::class, 'Google_Service_Datastream_CdcStrategy');
