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

namespace Google\Service\CloudComposer;

class PollAirflowCommandResponse extends \Google\Collection
{
  protected $collection_key = 'output';
  protected $exitInfoType = ExitInfo::class;
  protected $exitInfoDataType = '';
  protected $outputType = Line::class;
  protected $outputDataType = 'array';
  /**
   * @var bool
   */
  public $outputEnd;

  /**
   * @param ExitInfo
   */
  public function setExitInfo(ExitInfo $exitInfo)
  {
    $this->exitInfo = $exitInfo;
  }
  /**
   * @return ExitInfo
   */
  public function getExitInfo()
  {
    return $this->exitInfo;
  }
  /**
   * @param Line[]
   */
  public function setOutput($output)
  {
    $this->output = $output;
  }
  /**
   * @return Line[]
   */
  public function getOutput()
  {
    return $this->output;
  }
  /**
   * @param bool
   */
  public function setOutputEnd($outputEnd)
  {
    $this->outputEnd = $outputEnd;
  }
  /**
   * @return bool
   */
  public function getOutputEnd()
  {
    return $this->outputEnd;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PollAirflowCommandResponse::class, 'Google_Service_CloudComposer_PollAirflowCommandResponse');
