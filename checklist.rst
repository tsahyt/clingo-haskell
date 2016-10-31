Checklist to keep track of which parts of the Raw API are covered by the high-level API

Raw.Asynchronous
================
+ [X] 2016-10-25 solveAsyncCancel :: MonadIO m => AsyncSolver -> m CBool
+ [X] 2016-10-26 solveAsyncGet :: MonadIO m => AsyncSolver -> Ptr SolveResult -> m CBool
+ [X] 2016-10-25 solveAsyncWait :: MonadIO m => AsyncSolver -> CDouble -> Ptr CBool 

Raw.Basic
=========
+ [X] 2016-10-25 errorCode :: MonadIO m => m ClingoError
+ [X] 2016-10-25 errorMessage :: MonadIO m => m CString
+ [X] 2016-10-26 errorString :: MonadIO m => ClingoError -> m CString
+ [X] 2016-10-26 setError :: MonadIO m => ClingoError -> CString -> m ()
+ [X] 2016-10-25 version :: MonadIO m => Ptr CInt -> Ptr CInt -> Ptr CInt -> m ()
+ [X] 2016-10-26 warningString :: MonadIO m => ClingoWarning -> m CString

Raw.Configuration
=================
+ [X] 2016-10-29 configurationArrayAt :: MonadIO m => Configuration -> Identifier -> CSize -> Ptr Identifier -> m CBool
+ [X] 2016-10-29 configurationArraySize :: MonadIO m => Configuration -> Identifier -> Word64 -> Ptr CSize -> m CBool
+ [X] 2016-10-29 configurationDescription :: MonadIO m => Configuration -> Identifier -> Ptr CString -> m CBool
+ [X] 2016-10-29 configurationMapAt :: MonadIO m => Configuration -> Identifier -> CString -> Ptr Identifier -> m CBool
+ [X] 2016-10-29 configurationMapSize :: MonadIO m => Configuration -> Identifier -> Ptr CSize -> m CBool
+ [X] 2016-10-29 configurationMapSubkeyName :: MonadIO m => Configuration -> Identifier -> CSize -> Ptr CString -> m CBool
+ [X] 2016-10-29 configurationRoot :: MonadIO m => Configuration -> Ptr Identifier -> m CBool
+ [X] 2016-10-29 configurationType :: MonadIO m => Configuration -> Identifier -> Ptr ConfigurationType -> m CBool
+ [X] 2016-10-29 configurationValueGet :: MonadIO m => Configuration -> Identifier -> CString -> CSize -> m CBool
+ [X] 2016-10-29 configurationValueGetSize :: MonadIO m => Configuration -> Identifier -> Ptr CSize -> m CBool
+ [X] 2016-10-29 configurationValueIsAssigned :: MonadIO m => Configuration -> Identifier -> Ptr CBool -> m CBool
+ [X] 2016-10-29 configurationValueSet :: MonadIO m => Configuration -> Identifier -> CString -> m CBool

Raw.Control
===========
+ [X] 2016-10-27 controlAdd :: MonadIO m => Control -> CString -> Ptr CString -> CSize 
+ [X] 2016-10-27 controlAssignExternal :: MonadIO m => Control -> Symbol -> TruthValue 
+ [X] 2016-10-26 controlBackend :: MonadIO m => Control -> Ptr Backend -> m CBool
+ [X] 2016-10-31 controlClaspFacade :: MonadIO m => Control -> Ptr (Ptr ()) -> m CBool
+ [X] 2016-10-26 controlCleanup :: MonadIO m => Control -> m CBool
+ [X] 2016-10-26 controlConfiguration :: MonadIO m => Control -> Ptr Configuration
+ [X] 2016-10-25 controlFree :: MonadIO m => Control -> m ()
+ [X] 2016-10-27 controlGetConst :: MonadIO m => Control -> CString -> Ptr Symbol -> m CBool
+ [X] 2016-10-26 controlGround :: MonadIO m => Control -> Ptr Part -> CSize 
+ [X] 2016-10-27 controlHasConst :: MonadIO m => Control -> CString -> Ptr CBool -> m CBool
+ [X] 2016-10-26 controlInterrupt :: MonadIO m => Control -> m ()
+ [X] 2016-10-25 controlLoad :: MonadIO m => Control -> CString -> m CBool
+ [X] 2016-10-25 controlNew :: MonadIO m => Ptr CString -> CSize -> FunPtr (Logger a) 
+ [X] 2016-10-26 controlProgramBuilder :: MonadIO m => Control -> Ptr ProgramBuilder
+ [ ] controlRegisterObserver :: MonadIO m => Control 
+ [X] 2016-10-31 controlRegisterPropagator :: MonadIO m => Control -> Ptr (Propagator a) 
+ [X] 2016-10-27 controlReleaseExternal :: MonadIO m => Control -> Symbol -> m CBool
+ [X] 2016-10-26 controlSolve :: MonadIO m => Control -> FunPtr (CallbackModel a) -> Ptr a 
+ [X] 2016-10-26 controlSolveAsync :: MonadIO m => Control -> FunPtr (CallbackModel a) 
+ [X] 2016-10-26 controlSolveIter :: MonadIO m => Control -> Ptr SymbolicLiteral -> CSize 
+ [X] 2016-10-26 controlStatistics :: MonadIO m => Control -> Ptr Statistics -> m CBool
+ [X] 2016-10-26 controlSymbolicAtoms :: MonadIO m => Control -> Ptr SymbolicAtoms
+ [X] 2016-10-26 controlTheoryAtoms :: MonadIO m => Control -> Ptr TheoryAtoms
+ [X] 2016-10-27 controlUseEnumAssumption :: MonadIO m => Control -> CBool -> m CBool

Raw.Iterative
=============
+ [X] 2016-10-25 solveIterativelyClose :: MonadIO m 
+ [X] 2016-10-26 solveIterativelyGet :: MonadIO m 
+ [X] 2016-10-25 solveIterativelyNext :: MonadIO m 

Raw.Model
=========
+ [X] 2016-10-27 modelContains :: MonadIO m => Model -> Symbol -> Ptr CBool -> m CBool
+ [X] 2016-10-27 modelContext :: MonadIO m => Model -> Ptr SolveControl -> m CBool
+ [X] 2016-10-27 modelCost :: MonadIO m => Model -> Ptr Int64 -> CSize -> m CBool
+ [X] 2016-10-27 modelCostSize :: MonadIO m => Model -> Ptr CSize -> m CBool
+ [X] 2016-10-27 modelNumber :: MonadIO m => Model -> Ptr Word64 -> m CBool
+ [X] 2016-10-27 modelOptimalityProven :: MonadIO m => Model -> Ptr CBool -> m CBool
+ [X] 2016-10-27 modelSymbols :: MonadIO m => Model -> ShowFlag -> Ptr Symbol -> CSize 
+ [X] 2016-10-27 modelSymbolsSize :: MonadIO m => Model -> ShowFlag -> Ptr CSize -> m CBool
+ [X] 2016-10-27 modelType :: MonadIO m => Model -> ModelType -> m CBool
+ [X] 2016-10-27 solveControlAddClause :: MonadIO m => SolveControl -> Ptr SymbolicLiteral 
+ [X] 2016-10-27 solveControlThreadId :: MonadIO m => SolveControl -> Ptr Identifier 

Raw.ProgramBuilding
===================
+ [X] 2016-10-28 backendAcycEdge :: MonadIO m => Backend -> CInt -> CInt -> Ptr Literal -> CSize -> m CBool
+ [X] 2016-10-28 backendAddAtom :: MonadIO m => Backend -> Ptr Atom -> m CBool
+ [X] 2016-10-28 backendAssume :: MonadIO m => Backend -> Ptr Literal -> CSize -> m CBool
+ [ ] backendBuilderAdd :: MonadIO m => ProgramBuilder -> Ptr AstStatement -> m CBool
+ [ ] backendBuilderBegin :: MonadIO m => ProgramBuilder -> m CBool
+ [ ] backendBuilderEnd :: MonadIO m => ProgramBuilder -> m CBool
+ [X] 2016-10-28 backendExternal :: MonadIO m => Backend -> Atom -> ExternalType -> m CBool
+ [X] 2016-10-28 backendHeuristic :: MonadIO m => Backend -> Atom -> HeuristicType -> CInt -> CUInt -> Ptr Literal -> CSize -> m CBool
+ [X] 2016-10-28 backendMinimize :: MonadIO m => Backend -> Weight -> Ptr WeightedLiteral -> CSize -> m CBool
+ [X] 2016-10-28 backendProject :: MonadIO m => Backend -> Ptr Atom -> CSize -> m CBool
+ [X] 2016-10-28 backendRule :: MonadIO m => Backend -> CBool -> Ptr Atom -> CSize -> Ptr Literal -> CSize -> m CBool
+ [X] 2016-10-28 backendWeightRule :: MonadIO m => Backend -> CBool -> Ptr Atom -> CSize -> Weight -> Ptr WeightedLiteral -> CSize -> m CBool

Raw.Propagation
===============
+ [X] 2016-10-28 assignmentDecision :: MonadIO m => Assignment -> Word32 -> Ptr Literal -> m CBool
+ [X] 2016-10-28 assignmentDecisionLevel :: MonadIO m => Assignment -> m Word32
+ [X] 2016-10-28 assignmentHasConflict :: MonadIO m => Assignment -> m CBool
+ [X] 2016-10-28 assignmentHasLiteral :: MonadIO m => Assignment -> Literal -> m CBool
+ [X] 2016-10-28 assignmentIsFalse :: MonadIO m => Assignment -> Literal -> Ptr CBool -> m CBool
+ [X] 2016-10-28 assignmentIsFixed :: MonadIO m => Assignment -> Literal -> Ptr CBool -> m CBool
+ [X] 2016-10-28 assignmentIsTrue :: MonadIO m => Assignment -> Literal -> Ptr CBool -> m CBool
+ [X] 2016-10-28 assignmentLevel :: MonadIO m => Assignment -> Literal -> Ptr Word32 -> m CBool
+ [X] 2016-10-28 assignmentTruthValue :: MonadIO m => Assignment -> Literal -> Ptr TruthValue -> m CBool
+ [X] 2016-10-31 propagateControlAddClause :: MonadIO m => PropagateControl -> Ptr Literal -> CSize -> ClauseType -> Ptr CBool -> m CBool
+ [X] 2016-10-31 propagateControlAddLiteral :: MonadIO m => PropagateControl -> Ptr Literal -> m CBool
+ [X] 2016-10-31 propagateControlAddWatch :: MonadIO m => PropagateControl -> Literal -> m CBool
+ [X] 2016-10-31 propagateControlAssignment :: MonadIO m => PropagateControl -> m Assignment
+ [X] 2016-10-31 propagateControlHasWatch :: MonadIO m => PropagateControl -> Literal -> m CBool
+ [X] 2016-10-31 propagateControlPropagate :: MonadIO m => PropagateControl -> Ptr CBool -> m CBool
+ [X] 2016-10-31 propagateControlRemoveWatch :: MonadIO m => PropagateControl -> Literal -> m ()
+ [X] 2016-10-31 propagateControlThreadId :: MonadIO m => PropagateControl -> m Identifier
+ [X] 2016-10-31 propagateInitAddWatch :: MonadIO m => PropagateInit -> Literal -> m CBool
+ [X] 2016-10-31 propagateInitNumberOfThreads :: MonadIO m => PropagateInit -> m CInt
+ [X] 2016-10-31 propagateInitSolverLiteral :: MonadIO m => PropagateInit -> Literal -> Ptr Literal -> m CBool
+ [X] 2016-10-31 propagateInitSymbolicAtoms :: MonadIO m => PropagateInit -> Ptr SymbolicAtoms -> m CBool
+ [X] 2016-10-31 propagateInitTheoryAtoms :: MonadIO m => PropagateInit -> Ptr TheoryAtoms -> m CBool

Raw.Statistics
==============
+ [X] 2016-10-26 statisticsArrayAt :: MonadIO m => Statistics -> Word64 -> CSize 
+ [X] 2016-10-26 statisticsArraySize :: MonadIO m => Statistics -> Word64 -> Ptr Word64 
+ [X] 2016-10-26 statisticsMapAt :: MonadIO m => Statistics -> Word64 -> Ptr CString 
+ [X] 2016-10-26 statisticsMapSize :: MonadIO m => Statistics -> Word64 -> Ptr CSize 
+ [X] 2016-10-26 statisticsMapSubkeyName :: MonadIO m => Statistics -> Word64 -> CSize 
+ [X] 2016-10-26 statisticsRoot :: MonadIO m => Statistics -> Ptr Word64 -> m CBool
+ [X] 2016-10-26 statisticsType :: MonadIO m => Statistics -> Word64 -> Ptr StatisticsType 
+ [X] 2016-10-26 statisticsValueGet :: MonadIO m => Statistics -> Word64 -> Ptr CString 

Raw.Symbol
==========
+ [o] addString :: MonadIO m => CString -> Ptr CString -> m CBool
+ [X] 2016-10-26 parseTerm :: MonadIO m => CString -> FunPtr (Logger a) -> Ptr a -> CUInt 
+ [X] 2016-10-25 signatureArity :: Signature -> Word32
+ [X] 2016-10-26 signatureCreate :: MonadIO m => CString -> Word32 -> CBool -> Ptr Signature 
+ [X] 2016-10-25 signatureHash :: Signature -> CSize
+ [X] 2016-10-25 signatureIsEqualTo :: Signature -> Signature -> CBool
+ [X] 2016-10-25 signatureIsLessThan :: Signature -> Signature -> CBool
+ [X] 2016-10-25 signatureIsNegative :: Signature -> CBool
+ [X] 2016-10-25 signatureIsPositive :: Signature -> CBool
+ [X] 2016-10-25 signatureName :: Signature -> CString
+ [X] 2016-10-26 symbolArguments :: MonadIO m => Symbol -> Ptr (Ptr Symbol) -> Ptr CSize 
+ [X] 2016-10-26 symbolCreateFunction :: MonadIO m => CString -> Ptr Symbol -> CSize -> CBool 
+ [X] 2016-10-26 symbolCreateId :: MonadIO m => CString -> CBool -> Ptr Symbol -> m CBool
+ [X] 2016-10-25 symbolCreateInfimum :: MonadIO m => Ptr Symbol -> m ()
+ [X] 2016-10-25 symbolCreateNumber :: MonadIO m => CInt -> Ptr Symbol -> m ()
+ [X] 2016-10-26 symbolCreateString :: MonadIO m => CString -> Ptr Symbol -> m CBool
+ [X] 2016-10-25 symbolCreateSupremum :: MonadIO m => Ptr Symbol -> m ()
+ [X] 2016-10-25 symbolHash :: Symbol -> CSize
+ [X] 2016-10-25 symbolIsEqualTo :: Symbol -> Symbol -> CBool
+ [X] 2016-10-25 symbolIsLessThan :: Symbol -> Symbol -> CBool
+ [X] 2016-10-25 symbolIsNegative :: MonadIO m => Symbol -> Ptr CBool -> m CBool
+ [X] 2016-10-25 symbolIsPositive :: MonadIO m => Symbol -> Ptr CBool -> m CBool
+ [X] 2016-10-26 symbolName :: MonadIO m => Symbol -> Ptr CString -> m CBool
+ [X] 2016-10-26 symbolNumber :: MonadIO m => Symbol -> Ptr CInt -> m CBool
+ [X] 2016-10-26 symbolString :: MonadIO m => Symbol -> Ptr CString -> m CBool
+ [X] 2016-10-26 symbolSymbolToString :: MonadIO m => Symbol -> Ptr CChar -> m CBool
+ [X] 2016-10-26 symbolSymbolToStringSize :: MonadIO m => Symbol -> Ptr CSize -> m CBool
+ [X] 2016-10-25 symbolType :: Symbol -> SymbolType
