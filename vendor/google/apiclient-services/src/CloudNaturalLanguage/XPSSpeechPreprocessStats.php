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

namespace Google\Service\CloudNaturalLanguage;

class XPSSpeechPreprocessStats extends \Google\Collection
{
  protected $collection_key = 'dataErrors';
  protected $dataErrorsType = XPSDataErrors::class;
  protected $dataErrorsDataType = 'array';
  /**
   * @var int
   */
  public $numHumanLabeledExamples;
  /**
   * @var int
   */
  public $numLogsExamples;
  /**
   * @var int
   */
  public $numMachineTranscribedExamples;
  /**
   * @var int
   */
  public $testExamplesCount;
  /**
   * @var int
   */
  public $testSentencesCount;
  /**
   * @var int
   */
  public $testWordsCount;
  /**
   * @var int
   */
  public $trainExamplesCount;
  /**
   * @var int
   */
  public $trainSentencesCount;
  /**
   * @var int
   */
  public $trainWordsCount;

  /**
   * @param XPSDataErrors[]
   */
  public function setDataErrors($dataErrors)
  {
    $this->dataErrors = $dataErrors;
  }
  /**
   * @return XPSDataErrors[]
   */
  public function getDataErrors()
  {
    return $this->dataErrors;
  }
  /**
   * @param int
   */
  public function setNumHumanLabeledExamples($numHumanLabeledExamples)
  {
    $this->numHumanLabeledExamples = $numHumanLabeledExamples;
  }
  /**
   * @return int
   */
  public function getNumHumanLabeledExamples()
  {
    return $this->numHumanLabeledExamples;
  }
  /**
   * @param int
   */
  public function setNumLogsExamples($numLogsExamples)
  {
    $this->numLogsExamples = $numLogsExamples;
  }
  /**
   * @return int
   */
  public function getNumLogsExamples()
  {
    return $this->numLogsExamples;
  }
  /**
   * @param int
   */
  public function setNumMachineTranscribedExamples($numMachineTranscribedExamples)
  {
    $this->numMachineTranscribedExamples = $numMachineTranscribedExamples;
  }
  /**
   * @return int
   */
  public function getNumMachineTranscribedExamples()
  {
    return $this->numMachineTranscribedExamples;
  }
  /**
   * @param int
   */
  public function setTestExamplesCount($testExamplesCount)
  {
    $this->testExamplesCount = $testExamplesCount;
  }
  /**
   * @return int
   */
  public function getTestExamplesCount()
  {
    return $this->testExamplesCount;
  }
  /**
   * @param int
   */
  public function setTestSentencesCount($testSentencesCount)
  {
    $this->testSentencesCount = $testSentencesCount;
  }
  /**
   * @return int
   */
  public function getTestSentencesCount()
  {
    return $this->testSentencesCount;
  }
  /**
   * @param int
   */
  public function setTestWordsCount($testWordsCount)
  {
    $this->testWordsCount = $testWordsCount;
  }
  /**
   * @return int
   */
  public function getTestWordsCount()
  {
    return $this->testWordsCount;
  }
  /**
   * @param int
   */
  public function setTrainExamplesCount($trainExamplesCount)
  {
    $this->trainExamplesCount = $trainExamplesCount;
  }
  /**
   * @return int
   */
  public function getTrainExamplesCount()
  {
    return $this->trainExamplesCount;
  }
  /**
   * @param int
   */
  public function setTrainSentencesCount($trainSentencesCount)
  {
    $this->trainSentencesCount = $trainSentencesCount;
  }
  /**
   * @return int
   */
  public function getTrainSentencesCount()
  {
    return $this->trainSentencesCount;
  }
  /**
   * @param int
   */
  public function setTrainWordsCount($trainWordsCount)
  {
    $this->trainWordsCount = $trainWordsCount;
  }
  /**
   * @return int
   */
  public function getTrainWordsCount()
  {
    return $this->trainWordsCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSSpeechPreprocessStats::class, 'Google_Service_CloudNaturalLanguage_XPSSpeechPreprocessStats');
