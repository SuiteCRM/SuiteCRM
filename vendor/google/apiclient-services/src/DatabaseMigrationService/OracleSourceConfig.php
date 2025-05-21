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

namespace Google\Service\DatabaseMigrationService;

class OracleSourceConfig extends \Google\Model
{
  protected $binaryLogParserType = BinaryLogParser::class;
  protected $binaryLogParserDataType = '';
  /**
   * @var string
   */
  public $cdcStartPosition;
  protected $logMinerType = LogMiner::class;
  protected $logMinerDataType = '';
  /**
   * @var int
   */
  public $maxConcurrentCdcConnections;
  /**
   * @var int
   */
  public $maxConcurrentFullDumpConnections;
  /**
   * @var bool
   */
  public $skipFullDump;

  /**
   * @param BinaryLogParser
   */
  public function setBinaryLogParser(BinaryLogParser $binaryLogParser)
  {
    $this->binaryLogParser = $binaryLogParser;
  }
  /**
   * @return BinaryLogParser
   */
  public function getBinaryLogParser()
  {
    return $this->binaryLogParser;
  }
  /**
   * @param string
   */
  public function setCdcStartPosition($cdcStartPosition)
  {
    $this->cdcStartPosition = $cdcStartPosition;
  }
  /**
   * @return string
   */
  public function getCdcStartPosition()
  {
    return $this->cdcStartPosition;
  }
  /**
   * @param LogMiner
   */
  public function setLogMiner(LogMiner $logMiner)
  {
    $this->logMiner = $logMiner;
  }
  /**
   * @return LogMiner
   */
  public function getLogMiner()
  {
    return $this->logMiner;
  }
  /**
   * @param int
   */
  public function setMaxConcurrentCdcConnections($maxConcurrentCdcConnections)
  {
    $this->maxConcurrentCdcConnections = $maxConcurrentCdcConnections;
  }
  /**
   * @return int
   */
  public function getMaxConcurrentCdcConnections()
  {
    return $this->maxConcurrentCdcConnections;
  }
  /**
   * @param int
   */
  public function setMaxConcurrentFullDumpConnections($maxConcurrentFullDumpConnections)
  {
    $this->maxConcurrentFullDumpConnections = $maxConcurrentFullDumpConnections;
  }
  /**
   * @return int
   */
  public function getMaxConcurrentFullDumpConnections()
  {
    return $this->maxConcurrentFullDumpConnections;
  }
  /**
   * @param bool
   */
  public function setSkipFullDump($skipFullDump)
  {
    $this->skipFullDump = $skipFullDump;
  }
  /**
   * @return bool
   */
  public function getSkipFullDump()
  {
    return $this->skipFullDump;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(OracleSourceConfig::class, 'Google_Service_DatabaseMigrationService_OracleSourceConfig');
